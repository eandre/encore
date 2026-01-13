//! Schema conversion for Python types to protobuf schema types.
//!
//! This module converts Python parser types to the protobuf schema representation
//! used by the Encore runtime and other tools.

use std::collections::HashMap;
use std::path::Path;

use crate::ast::loader::errors::{ParseError, ParseResult};
use crate::ast::loader::fileset::{FilePath, FileSet, Span};
use crate::ast::loader::ModuleId;
use crate::ast::schema::object::ObjectKind;
use crate::ast::schema::{Basic, ClassType, Field, Literal, Named, Parser, Type, TypedDict};
use crate::encore::parser::schema::v1 as schema;
use crate::encore::parser::schema::v1::r#type as styp;

/// SchemaBuilder manages the conversion of Python types to protobuf schema types.
pub struct SchemaBuilder<'a, 'py> {
    file_set: &'a FileSet,
    app_root: &'a Path,
    parser: &'py Parser<'py>,

    decls: Vec<schema::Decl>,
    name_to_decl: HashMap<(ModuleId, String), u32>,
}

impl<'a, 'py> SchemaBuilder<'a, 'py> {
    /// Create a new SchemaBuilder.
    pub fn new(file_set: &'a FileSet, app_root: &'a Path, parser: &'py Parser<'py>) -> Self {
        SchemaBuilder {
            file_set,
            app_root,
            parser,
            decls: Vec::new(),
            name_to_decl: HashMap::new(),
        }
    }

    /// Consume the builder and return the collected declarations.
    pub fn into_decls(self) -> Vec<schema::Decl> {
        self.decls
    }

    /// Convert a Python type to a schema type.
    /// The span is used for error reporting.
    pub fn typ(&mut self, typ: &Type<'py>, span: Span) -> ParseResult<schema::Type> {
        Ok(match typ {
            Type::Basic(basic) => self.basic(basic),

            Type::List(elem) => {
                let elem_typ = self.typ(elem, span)?;
                schema::Type {
                    typ: Some(styp::Typ::List(Box::new(schema::List {
                        elem: Some(Box::new(elem_typ)),
                    }))),
                    validation: None,
                }
            }

            Type::Dict(key, value) => {
                let key_typ = self.typ(key, span)?;
                let value_typ = self.typ(value, span)?;
                schema::Type {
                    typ: Some(styp::Typ::Map(Box::new(schema::Map {
                        key: Some(Box::new(key_typ)),
                        value: Some(Box::new(value_typ)),
                    }))),
                    validation: None,
                }
            }

            Type::Union(types) => {
                let mut schema_types = Vec::with_capacity(types.len());
                for t in types {
                    schema_types.push(self.typ(t, span)?);
                }
                schema::Type {
                    typ: Some(styp::Typ::Union(schema::Union {
                        types: schema_types,
                    })),
                    validation: None,
                }
            }

            Type::Tuple(_) => {
                return Err(ParseError::Parse {
                    span,
                    message: "tuple types are not supported in schemas".to_string(),
                });
            }

            Type::Literal(lit) => schema::Type {
                typ: Some(styp::Typ::Literal(self.literal(lit))),
                validation: None,
            },

            Type::Named(named) => {
                let named_ref = self.named(named, span)?;
                schema::Type {
                    typ: Some(styp::Typ::Named(named_ref)),
                    validation: None,
                }
            }

            Type::TypeParamRefType(_) => {
                // Type parameter reference - would need a decl_id context.
                // For now, treat as Any.
                schema::Type {
                    typ: Some(styp::Typ::Builtin(schema::Builtin::Any as i32)),
                    validation: None,
                }
            }

            Type::TypedDict(td) => self.typed_dict(td, span)?,

            Type::ClassType(ct) => self.class_type(ct, span)?,
        })
    }

    /// Convert a basic Python type to a schema type.
    fn basic(&self, typ: &Basic) -> schema::Type {
        let builtin = |b: schema::Builtin| schema::Type {
            typ: Some(styp::Typ::Builtin(b as i32)),
            validation: None,
        };

        match typ {
            Basic::Str => builtin(schema::Builtin::String),
            Basic::Int => builtin(schema::Builtin::Int64),
            Basic::Float => builtin(schema::Builtin::Float64),
            Basic::Bool => builtin(schema::Builtin::Bool),
            Basic::Bytes => builtin(schema::Builtin::Bytes),
            Basic::None => schema::Type {
                typ: Some(styp::Typ::Literal(schema::Literal {
                    value: Some(schema::literal::Value::Null(true)),
                })),
                validation: None,
            },
            Basic::Any => builtin(schema::Builtin::Any),
            Basic::Never => builtin(schema::Builtin::Any), // No direct mapping for Never
        }
    }

    /// Convert a Python literal to a schema literal.
    fn literal(&self, lit: &Literal) -> schema::Literal {
        use schema::literal::Value;
        let val = match lit {
            Literal::String(s) => Value::Str(s.clone()),
            Literal::Boolean(b) => Value::Boolean(*b),
            Literal::Int(i) => Value::Int(*i),
            Literal::Float(f) => Value::Float(*f),
            Literal::BigInt(s) => Value::Str(s.clone()),
        };
        schema::Literal { value: Some(val) }
    }

    /// Convert a Named type reference to a schema Named reference.
    /// Creates a declaration for the named type if one doesn't exist.
    fn named(&mut self, named: &Named<'py>, span: Span) -> ParseResult<schema::Named> {
        let obj = named.obj;
        let name = obj.name().to_string();
        let module = obj.module();
        let decl_key = (module.id, name.clone());

        // Convert type arguments
        let type_arguments: Vec<schema::Type> = named
            .type_args
            .iter()
            .map(|t| self.typ(t, span))
            .collect::<ParseResult<Vec<_>>>()?;

        // Check if we already have a declaration for this type
        if let Some(&decl_id) = self.name_to_decl.get(&decl_key) {
            return Ok(schema::Named {
                id: decl_id,
                type_arguments,
            });
        }

        // Allocate a new declaration ID
        let decl_id = self.decls.len() as u32;
        self.name_to_decl.insert(decl_key, decl_id);

        // Create location from object span
        let loc = self.loc_from_span(obj.span()).ok();

        // Create an empty declaration first (to handle recursive types)
        // We'll fill in the type after resolving it
        self.decls.push(schema::Decl {
            id: decl_id,
            name: name.clone(),
            r#type: None,
            type_params: vec![],
            doc: String::new(),
            loc,
        });

        // Resolve the underlying type from the class definition
        let underlying = self.resolve_named_type(&name, module, span)?;

        // Update the declaration with the resolved type
        if let Some(decl) = self.decls.get_mut(decl_id as usize) {
            decl.r#type = Some(underlying);
        }

        Ok(schema::Named {
            id: decl_id,
            type_arguments,
        })
    }

    /// Resolve the underlying type of a named type by parsing its class definition.
    fn resolve_named_type(
        &mut self,
        name: &str,
        module: &'py crate::ast::loader::Module,
        span: Span,
    ) -> ParseResult<schema::Type> {
        // Inject the module into the parser to get the ModuleObj
        let module_obj = self.parser.inject_module(module);

        // Look up the object in the module's top_level
        match module_obj.top_level.get(name) {
            Some(ObjectKind::Class(class)) => {
                // Parse the class definition to get the underlying type
                let parsed_type = self.parser.parse_class_def(module_obj, class.node)?;

                // Convert the parsed type to schema type
                self.typ(&parsed_type, span)
            }
            Some(ObjectKind::Func(_)) => {
                // Functions can't be used as types
                Err(ParseError::Parse {
                    span,
                    message: format!("'{}' is a function, not a type", name),
                })
            }
            Some(ObjectKind::Module(_)) => {
                // Modules can't be used as types
                Err(ParseError::Parse {
                    span,
                    message: format!("'{}' is a module, not a type", name),
                })
            }
            Some(ObjectKind::Var(_)) => {
                // Variables can't be used as types (yet - type aliases would need special handling)
                Err(ParseError::Parse {
                    span,
                    message: format!("'{}' is a variable, not a type", name),
                })
            }
            None => {
                // Unknown type - return empty struct as fallback
                Ok(schema::Type {
                    typ: Some(styp::Typ::Struct(schema::Struct { fields: vec![] })),
                    validation: None,
                })
            }
        }
    }

    /// Create a schema location from a span.
    fn loc_from_span(&self, span: Span) -> ParseResult<schema::Loc> {
        let file_id = span.file_id();
        let file = self.file_set.get_by_id(file_id);

        let (pkg_path, pkg_name, filename) = match &file.path {
            FilePath::Custom(s) => {
                return Err(ParseError::Parse {
                    span,
                    message: format!("unsupported file path: {}", s),
                });
            }
            FilePath::Real(buf) => {
                let rel_path = buf.strip_prefix(self.app_root).unwrap_or(buf);
                let file_name = rel_path
                    .file_name()
                    .map(|s| s.to_string_lossy().to_string())
                    .unwrap_or_default();
                let pkg_name = rel_path
                    .parent()
                    .and_then(|p| p.file_name())
                    .or_else(|| self.app_root.file_name())
                    .map(|s| s.to_string_lossy().to_string())
                    .unwrap_or_default();
                let pkg_path = rel_path
                    .parent()
                    .map(|s| s.to_string_lossy().to_string())
                    .unwrap_or_else(|| ".".to_string());
                (pkg_path, pkg_name, file_name)
            }
        };

        Ok(schema::Loc {
            pkg_path,
            pkg_name,
            filename,
            start_pos: span.start() as i32,
            end_pos: span.end() as i32,
            src_line_start: 0,
            src_line_end: 0,
            src_col_start: 0,
            src_col_end: 0,
        })
    }

    /// Convert a TypedDict to a schema struct type.
    fn typed_dict(&mut self, td: &TypedDict<'py>, span: Span) -> ParseResult<schema::Type> {
        let fields = self.convert_fields(&td.fields, !td.total, span)?;
        Ok(schema::Type {
            typ: Some(styp::Typ::Struct(schema::Struct { fields })),
            validation: None,
        })
    }

    /// Convert a ClassType to a schema struct type.
    fn class_type(&mut self, ct: &ClassType<'py>, span: Span) -> ParseResult<schema::Type> {
        let fields = self.convert_fields(&ct.fields, false, span)?;
        Ok(schema::Type {
            typ: Some(styp::Typ::Struct(schema::Struct { fields })),
            validation: None,
        })
    }

    /// Convert a list of fields to schema fields.
    fn convert_fields(
        &mut self,
        fields: &[Field<'py>],
        all_optional: bool,
        span: Span,
    ) -> ParseResult<Vec<schema::Field>> {
        let mut result = Vec::with_capacity(fields.len());

        for field in fields {
            let field_typ = self.typ(&field.typ, span)?;
            let optional = field.optional || all_optional;

            let mut tags = vec![];
            if optional {
                tags.push(schema::Tag {
                    key: "encore".into(),
                    name: "optional".into(),
                    options: vec![],
                });
            }

            let raw_tag = if optional {
                r#"encore:"optional""#.to_string()
            } else {
                String::new()
            };

            result.push(schema::Field {
                typ: Some(field_typ),
                name: field.name.clone(),
                json_name: field.name.clone(),
                optional,
                wire: None,
                tags,
                raw_tag,
                query_string_name: String::new(),
                doc: field.doc.clone().unwrap_or_default(),
            });
        }

        Ok(result)
    }
}
