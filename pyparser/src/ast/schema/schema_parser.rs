//! Schema parser for Python types into Encore's schema format.
//!
//! This module implements parsing of Python types, modeled after the Go implementation
//! in `v2/internals/schema/schema_parser.go`.

use std::collections::HashMap;

use super::typ::{Basic, ClassType, Field, Literal, Named, Type, TypedDict};

use crate::ast::loader::modpath::ModulePath;
use crate::ast::loader::{
    errors::{ParseError, ParseResult},
    parsectx, Module, ModuleLoader,
};
use crate::ast::schema::object::{ModuleObj, Object, ObjectKind};
use memo_map::MemoMap;
use ruff_python_ast as ast;
use ruff_text_size::Ranged;

/// Parser parses Python types into Encore's schema format.
pub struct Parser<'py> {
    #[allow(dead_code)]
    ctx: &'py parsectx::Context,
    loader: &'py ModuleLoader,
    modules: MemoMap<ModulePath, ModuleObj<'py>>,
}

impl<'py> Parser<'py> {
    /// Creates a new schema parser.
    pub fn new(ctx: &'py parsectx::Context, loader: &'py ModuleLoader) -> Self {
        Parser {
            ctx,
            loader,
            modules: MemoMap::new(),
        }
    }

    /// Injects a module into the parser's cache.
    pub fn inject_module(&self, module: &'py Module) -> &ModuleObj<'py> {
        let mod_path = &module.mod_path;
        self.modules
            .get_or_insert(mod_path, || ModuleObj::new(module))
    }

    /// Resolves a module by its path, loading it if necessary.
    pub fn resolve_module(&self, mod_path: &'_ ModulePath) -> ParseResult<&ModuleObj<'py>> {
        let module = self.loader.get_or_resolve(mod_path)?;

        let obj = self
            .modules
            .get_or_insert(mod_path, || ModuleObj::new(module));
        Ok(obj)
    }

    /// Creates a new type resolver for parsing types within a declaration context.
    fn new_type_resolver(
        &'py self,
        module_obj: &'py ModuleObj<'py>,
        type_params_in_scope: Option<HashMap<String, usize>>,
    ) -> TypeResolver<'py, 'py> {
        TypeResolver {
            parser: self,
            module_obj,
            type_params_in_scope,
        }
    }

    /// Parses a type expression within the context of a module.
    pub fn parse_type(
        &'py self,
        module_obj: &'py ModuleObj<'py>,
        expr: &ast::Expr,
    ) -> ParseResult<Type<'py>> {
        let mut resolver = self.new_type_resolver(module_obj, None);
        resolver.parse_type(expr)
    }

    /// Resolves an expression to the object it refers to.
    ///
    /// This works by computing the type of the expression, and if it's a Named type,
    /// returns the underlying object.
    pub fn resolve_obj(
        &'py self,
        module_obj: &'py ModuleObj<'py>,
        expr: &ast::Expr,
    ) -> ParseResult<Option<&'py dyn Object>> {
        let typ = self.parse_type(module_obj, expr)?;
        Ok(match typ {
            Type::Named(named) => Some(named.obj),
            _ => None,
        })
    }

    /// Parses a class definition within the context of a module.
    ///
    /// The `module_obj` is used to check if TypedDict is imported from the typing module.
    pub fn parse_class_def(
        &'py self,
        module_obj: &'py ModuleObj<'py>,
        class_def: &ast::StmtClassDef,
    ) -> ParseResult<Type<'py>> {
        let mut resolver = self.new_type_resolver(module_obj, None);

        // Create a closure that checks if a name refers to TypedDict
        let is_typed_dict_imported = |name: &str| -> bool {
            // Check if it's imported from typing or typing_extensions
            module_obj.is_imported_from(name, "typing")
                || module_obj.is_imported_from(name, "typing_extensions")
        };

        resolver.parse_class_def(class_def, is_typed_dict_imported)
    }
}

/// TypeResolver resolves types from AST expressions.
///
/// This is a helper struct used internally by the Parser for parsing types
/// within a specific declaration context. It tracks type parameters that are
/// in scope for the current declaration being parsed.
struct TypeResolver<'r, 'py> {
    /// The parser that created this type resolver.
    #[allow(dead_code)]
    parser: &'r Parser<'py>,

    /// The module object for looking up top-level names.
    module_obj: &'r ModuleObj<'py>,

    /// type_params_in_scope contains the in-scope type parameters
    /// for the current declaration being parsed.
    ///
    /// The keys are the names of the type parameter,
    /// and the values are their index in the type parameter declaration list.
    type_params_in_scope: Option<HashMap<String, usize>>,
}

impl<'r: 'py, 'py> TypeResolver<'r, 'py> {
    /// Parses a type expression and returns it.
    pub fn parse_type(&mut self, expr: &ast::Expr) -> ParseResult<Type<'py>> {
        use ast::Expr;

        Ok(match expr {
            Expr::StringLiteral(x) => Type::Literal(Literal::String(x.value.to_string())),
            Expr::NumberLiteral(x) => match &x.value {
                ast::Number::Int(i) => match i.as_i64() {
                    Some(i) => Type::Literal(Literal::Int(i)),
                    None => Type::Literal(Literal::BigInt(i.to_string())),
                },
                ast::Number::Float(f) => Type::Literal(Literal::Float(*f)),
                ast::Number::Complex { .. } => Err(self.err(
                    expr,
                    "complex literals are not supported in type annotations",
                ))?,
            },
            Expr::BooleanLiteral(x) => Type::Literal(Literal::Boolean(x.value)),
            Expr::NoneLiteral(_) => Type::Basic(Basic::None),
            Expr::Name(name) => self.parse_name(name)?,

            Expr::Subscript(_x) => Err(self.err(expr, "subscript types not yet implemented"))?,
            Expr::Tuple(_x) => Err(self.err(expr, "tuple types not yet implemented"))?,

            Expr::BoolOp(_)
            | Expr::Named(_)
            | Expr::BinOp(_)
            | Expr::UnaryOp(_)
            | Expr::Lambda(_)
            | Expr::If(_)
            | Expr::Dict(_)
            | Expr::Set(_)
            | Expr::ListComp(_)
            | Expr::SetComp(_)
            | Expr::DictComp(_)
            | Expr::Generator(_)
            | Expr::Await(_)
            | Expr::Yield(_)
            | Expr::YieldFrom(_)
            | Expr::Compare(_)
            | Expr::Call(_)
            | Expr::FString(_)
            | Expr::BytesLiteral(_)
            | Expr::EllipsisLiteral(_)
            | Expr::Attribute(_)
            | Expr::Starred(_)
            | Expr::List(_)
            | Expr::Slice(_)
            | Expr::IpyEscapeCommand(_) => {
                Err(self.err(expr, "unexpected type for type annotation"))?
            }
        })
    }

    /// Parses a name expression into a type.
    ///
    /// This handles:
    /// - Built-in types (int, str, float, etc.)
    /// - Type parameters in scope
    /// - Named types from the module's top-level objects
    fn parse_name(&self, name: &ast::ExprName) -> ParseResult<Type<'py>> {
        let name_str = name.id.as_str();

        // Check for built-in types first
        match name_str {
            "int" => return Ok(Type::Basic(Basic::Int)),
            "str" => return Ok(Type::Basic(Basic::Str)),
            "float" => return Ok(Type::Basic(Basic::Float)),
            "bool" => return Ok(Type::Basic(Basic::Bool)),
            "bytes" => return Ok(Type::Basic(Basic::Bytes)),
            "Any" => return Ok(Type::Basic(Basic::Any)),
            "None" => return Ok(Type::Basic(Basic::None)),
            "never" => return Ok(Type::Basic(Basic::Never)),
            _ => {}
        }

        // Check if it's a type parameter in scope
        if let Some(type_params) = &self.type_params_in_scope {
            if let Some(&index) = type_params.get(name_str) {
                return Ok(Type::TypeParamRefType(index));
            }
        }

        // Check if it's a top-level object in the module
        if let Some(obj_kind) = self.module_obj.top_level.get(name_str) {
            let obj: &'py dyn Object = match obj_kind {
                ObjectKind::Class(class) => class,
                ObjectKind::Func(func) => func,
                ObjectKind::Var(var) => var,
                ObjectKind::Module(module) => module,
            };

            return Ok(Type::Named(Named {
                obj,
                type_args: Vec::new(),
            }));
        }

        // Unknown type
        Err(self.err(name, format!("unknown type '{}'", name_str)))
    }

    /// Parses a class definition and returns its type representation.
    ///
    /// This handles:
    /// - TypedDict classes (classes inheriting from TypedDict)
    /// - Regular classes with typed fields (ClassType)
    ///
    /// The `is_typed_dict_imported` closure should check if a given name
    /// refers to TypedDict from the typing module.
    pub fn parse_class_def(
        &mut self,
        class_def: &ast::StmtClassDef,
        is_typed_dict_imported: impl Fn(&str) -> bool,
    ) -> ParseResult<Type<'py>> {
        let name = class_def.name.to_string();

        // Check if this class inherits from TypedDict
        let is_typed_dict = self.is_typed_dict_class(class_def, &is_typed_dict_imported);

        // Check for total=False in class arguments (only relevant for TypedDict)
        let total = if is_typed_dict {
            self.get_typed_dict_total(class_def)
        } else {
            true
        };

        // Parse the fields from the class body
        let fields = self.parse_class_fields(&class_def.body, !total)?;

        if is_typed_dict {
            Ok(Type::TypedDict(TypedDict {
                name,
                fields,
                total,
            }))
        } else {
            Ok(Type::ClassType(ClassType { name, fields }))
        }
    }

    /// Checks if a class definition inherits from TypedDict.
    fn is_typed_dict_class(
        &self,
        class_def: &ast::StmtClassDef,
        is_typed_dict_imported: &impl Fn(&str) -> bool,
    ) -> bool {
        for base in class_def.bases() {
            match base {
                // Simple name: `class Foo(TypedDict):`
                ast::Expr::Name(name) => {
                    if is_typed_dict_imported(name.id.as_str()) {
                        return true;
                    }
                }
                // Attribute access: `class Foo(typing.TypedDict):`
                ast::Expr::Attribute(attr) => {
                    if attr.attr.as_str() == "TypedDict" {
                        return true;
                    }
                }
                _ => {}
            }
        }
        false
    }

    /// Gets the `total` parameter from a TypedDict class definition.
    /// Returns true by default (all fields required), false if `total=False` is specified.
    fn get_typed_dict_total(&self, class_def: &ast::StmtClassDef) -> bool {
        for keyword in class_def.keywords() {
            if let Some(arg) = &keyword.arg {
                if arg.as_str() == "total" {
                    if let ast::Expr::BooleanLiteral(b) = &keyword.value {
                        return b.value;
                    }
                }
            }
        }
        true // Default is total=True
    }

    /// Parses fields from a class body.
    /// Fields are annotated assignments like `name: str` or `name: str = "default"`.
    fn parse_class_fields(
        &mut self,
        body: &[ast::Stmt],
        all_optional: bool,
    ) -> ParseResult<Vec<Field<'py>>> {
        let mut fields = Vec::new();

        for stmt in body {
            // Look for annotated assignments: `name: Type` or `name: Type = value`
            if let ast::Stmt::AnnAssign(ann_assign) = stmt {
                // The target should be a simple name
                if let ast::Expr::Name(name) = &*ann_assign.target {
                    let field_name = name.id.to_string();

                    // Skip private fields (starting with _)
                    if field_name.starts_with('_') {
                        continue;
                    }

                    // Parse the type annotation
                    let typ = self.parse_type(&ann_assign.annotation)?;

                    // Field is optional if:
                    // - It has a default value
                    // - The TypedDict has total=False (all_optional)
                    // - The type is Optional[T] or T | None (handled separately)
                    let optional = all_optional || ann_assign.value.is_some();

                    fields.push(Field {
                        name: field_name,
                        typ,
                        optional,
                        doc: None, // TODO: Extract docstrings if present
                    });
                }
            }
        }

        Ok(fields)
    }

    /// Creates a parse error at the given location.
    fn err(&self, ranged: impl Ranged, msg: impl Into<String>) -> ParseError {
        ParseError::Parse {
            span: self.module_obj.module.file.id.to_span(ranged),
            message: msg.into(),
        }
    }
}
