//! Tests for Python type resolution.
//!
//! These tests use insta for snapshot testing. To update snapshots:
//! ```
//! cargo insta test --review
//! ```

use std::fs;
use std::sync::Arc;

use indexmap::IndexMap;
use insta::{assert_debug_snapshot, glob};
use ruff_python_ast as ast;
use ruff_python_parser::parse_module;

use super::object::{Class, ModuleData, ObjectKind, ResolveState};
use super::typ::Type;
use super::type_resolve::Ctx;

/// A resolved type entry for snapshot testing.
#[derive(Debug)]
struct ResolvedEntry {
    name: String,
    typ: Type,
}

/// Extract and resolve all type annotations from a Python source file.
fn resolve_types_from_source(source: &str) -> IndexMap<String, Type> {
    let parsed = parse_module(source).expect("Failed to parse Python source");
    let state = ResolveState::new();
    let module_id = state.new_module_id();

    // Register an empty module
    state.register_module(module_id, Arc::new(ModuleData::default()));

    let ctx = Ctx::new(&state, module_id);

    let mut results = IndexMap::new();

    for stmt in parsed.suite() {
        match stmt {
            // Annotated assignment: name: Type or name: Type = value
            ast::Stmt::AnnAssign(ann) => {
                if let ast::Expr::Name(name) = ann.target.as_ref() {
                    let typ = ctx.resolve_expr(&ann.annotation);
                    results.insert(name.id.to_string(), typ);
                }
            }

            // Simple assignment that might be a type alias: Name = Type
            ast::Stmt::Assign(assign) => {
                if assign.targets.len() == 1 {
                    if let ast::Expr::Name(name) = &assign.targets[0] {
                        // Try to resolve as type expression
                        let typ = ctx.resolve_expr(&assign.value);
                        results.insert(name.id.to_string(), typ);
                    }
                }
            }

            // Type alias statement (PEP 695): type Name = Type
            ast::Stmt::TypeAlias(alias) => {
                if let ast::Expr::Name(name) = alias.name.as_ref() {
                    let typ = ctx.resolve_expr(&alias.value);
                    results.insert(name.id.to_string(), typ);
                }
            }

            // Function definition
            ast::Stmt::FunctionDef(func) => {
                let func_type = resolve_function_type(&ctx, func);
                results.insert(func.name.to_string(), func_type);
            }

            // Class definition
            ast::Stmt::ClassDef(class) => {
                let class_info = resolve_class_info(&ctx, class);
                results.insert(class.name.to_string(), class_info);
            }

            _ => {}
        }
    }

    results
}

/// Resolve a function definition to its type.
fn resolve_function_type(ctx: &Ctx<'_>, func: &ast::StmtFunctionDef) -> Type {
    use super::typ::{FunctionParam, FunctionType, ParamKind};

    let mut params = Vec::new();

    // Positional-only parameters
    for param in &func.parameters.posonlyargs {
        let typ = param
            .parameter
            .annotation
            .as_ref()
            .map(|a| ctx.resolve_expr(a))
            .unwrap_or(Type::Any);
        params.push(FunctionParam {
            name: Some(param.parameter.name.to_string()),
            typ,
            optional: param.default.is_some(),
            variadic: false,
            keyword_variadic: false,
            kind: ParamKind::PositionalOnly,
        });
    }

    // Regular parameters
    for param in &func.parameters.args {
        let typ = param
            .parameter
            .annotation
            .as_ref()
            .map(|a| ctx.resolve_expr(a))
            .unwrap_or(Type::Any);
        params.push(FunctionParam {
            name: Some(param.parameter.name.to_string()),
            typ,
            optional: param.default.is_some(),
            variadic: false,
            keyword_variadic: false,
            kind: ParamKind::Regular,
        });
    }

    // *args
    if let Some(vararg) = &func.parameters.vararg {
        let typ = vararg
            .annotation
            .as_ref()
            .map(|a| ctx.resolve_expr(a))
            .unwrap_or(Type::Any);
        params.push(FunctionParam {
            name: Some(vararg.name.to_string()),
            typ,
            optional: false,
            variadic: true,
            keyword_variadic: false,
            kind: ParamKind::Regular,
        });
    }

    // Keyword-only parameters
    for param in &func.parameters.kwonlyargs {
        let typ = param
            .parameter
            .annotation
            .as_ref()
            .map(|a| ctx.resolve_expr(a))
            .unwrap_or(Type::Any);
        params.push(FunctionParam {
            name: Some(param.parameter.name.to_string()),
            typ,
            optional: param.default.is_some(),
            variadic: false,
            keyword_variadic: false,
            kind: ParamKind::KeywordOnly,
        });
    }

    // **kwargs
    if let Some(kwarg) = &func.parameters.kwarg {
        let typ = kwarg
            .annotation
            .as_ref()
            .map(|a| ctx.resolve_expr(a))
            .unwrap_or(Type::Any);
        params.push(FunctionParam {
            name: Some(kwarg.name.to_string()),
            typ,
            optional: false,
            variadic: false,
            keyword_variadic: true,
            kind: ParamKind::Regular,
        });
    }

    // Return type
    let return_type = func
        .returns
        .as_ref()
        .map(|r| ctx.resolve_expr(r))
        .unwrap_or(Type::Any);

    Type::Callable(FunctionType {
        params,
        return_type: Box::new(return_type),
        type_params: None,
        is_async: func.is_async,
    })
}

/// Check if a class inherits from TypedDict.
fn is_typed_dict(class: &ast::StmtClassDef) -> bool {
    class.arguments.as_ref().map_or(false, |args| {
        args.args.iter().any(|arg| {
            matches!(arg, ast::Expr::Name(n) if n.id.as_str() == "TypedDict")
        })
    })
}

/// Check if a TypedDict has total=False in its arguments.
fn is_typed_dict_partial(class: &ast::StmtClassDef) -> bool {
    class.arguments.as_ref().map_or(false, |args| {
        args.keywords.iter().any(|kw| {
            kw.arg.as_ref().map_or(false, |arg| arg.as_str() == "total")
                && matches!(&kw.value, ast::Expr::BooleanLiteral(b) if !b.value)
        })
    })
}

/// Resolve a TypedDict annotation, handling Required[T] and NotRequired[T].
fn resolve_typed_dict_field(ctx: &Ctx<'_>, annotation: &ast::Expr, total: bool) -> (Type, bool) {
    // Check for Required[T] or NotRequired[T] wrapper
    if let ast::Expr::Subscript(sub) = annotation {
        if let ast::Expr::Name(name) = sub.value.as_ref() {
            match name.id.as_str() {
                "Required" => {
                    let inner_type = ctx.resolve_expr(&sub.slice);
                    return (inner_type, false); // Required = not optional
                }
                "NotRequired" => {
                    let inner_type = ctx.resolve_expr(&sub.slice);
                    return (inner_type, true); // NotRequired = optional
                }
                _ => {}
            }
        }
    }
    // Default: optionality depends on total
    let typ = ctx.resolve_expr(annotation);
    (typ, !total) // If total=True, fields are required (not optional); if total=False, fields are optional
}

/// Resolve a TypedDict class as an Interface type.
fn resolve_typed_dict(ctx: &Ctx<'_>, class: &ast::StmtClassDef) -> Type {
    use super::typ::{FieldName, Interface, InterfaceField};

    let total = !is_typed_dict_partial(class);
    let mut fields = Vec::new();

    for stmt in &class.body {
        if let ast::Stmt::AnnAssign(ann) = stmt {
            if let ast::Expr::Name(name) = ann.target.as_ref() {
                let (typ, optional) = resolve_typed_dict_field(ctx, &ann.annotation, total);
                fields.push(InterfaceField {
                    range: ann.range,
                    name: FieldName::String(name.id.to_string()),
                    optional,
                    readonly: false,
                    typ,
                    doc: None,
                });
            }
        }
    }

    Type::Interface(Interface {
        fields,
        index_signature: None,
        total,
    })
}

/// Resolve a class definition to extract type information.
fn resolve_class_info(ctx: &Ctx<'_>, class: &ast::StmtClassDef) -> Type {
    // Check if this is a TypedDict - parse as Interface instead of Class
    if is_typed_dict(class) {
        return resolve_typed_dict(ctx, class);
    }

    use super::typ::{ClassType, FieldName, InterfaceField, MethodType};

    let state = ResolveState::new();
    let module_id = state.new_module_id();

    // Create an object for the class
    let obj = state.create_object(
        class.range,
        Some(class.name.to_string()),
        ObjectKind::Class(Class {
            bases: class.arguments.as_ref().map_or(vec![], |args| {
                args.args.iter().cloned().collect()
            }),
            body: class.body.clone(),
            type_params: vec![],
            decorators: class.decorator_list.clone(),
            is_dataclass: class
                .decorator_list
                .iter()
                .any(|d| matches!(&d.expression, ast::Expr::Name(n) if n.id.as_str() == "dataclass")),
            is_named_tuple: false,
        }),
        module_id,
    );

    // Extract instance attributes from class body
    let mut instance_attrs = Vec::new();
    let mut methods = Vec::new();

    for stmt in &class.body {
        match stmt {
            ast::Stmt::AnnAssign(ann) => {
                if let ast::Expr::Name(name) = ann.target.as_ref() {
                    let typ = ctx.resolve_expr(&ann.annotation);
                    instance_attrs.push(InterfaceField {
                        range: ann.range,
                        name: FieldName::String(name.id.to_string()),
                        optional: ann.value.is_some(),
                        readonly: false,
                        typ,
                        doc: None,
                    });
                }
            }
            ast::Stmt::FunctionDef(func) => {
                let sig = resolve_function_type(ctx, func);
                if let Type::Callable(func_type) = sig {
                    let is_classmethod = func.decorator_list.iter().any(|d| {
                        matches!(&d.expression, ast::Expr::Name(n) if n.id.as_str() == "classmethod")
                    });
                    let is_staticmethod = func.decorator_list.iter().any(|d| {
                        matches!(&d.expression, ast::Expr::Name(n) if n.id.as_str() == "staticmethod")
                    });
                    let is_property = func.decorator_list.iter().any(|d| {
                        matches!(&d.expression, ast::Expr::Name(n) if n.id.as_str() == "property")
                    });
                    methods.push(MethodType {
                        name: func.name.to_string(),
                        signature: func_type,
                        is_classmethod,
                        is_staticmethod,
                        is_property,
                    });
                }
            }
            _ => {}
        }
    }

    // Resolve base classes
    let bases: Vec<Type> = class
        .arguments
        .as_ref()
        .map_or(vec![], |args| {
            args.args.iter().map(|a| ctx.resolve_expr(a)).collect()
        });

    Type::Class(ClassType {
        obj,
        type_arguments: vec![],
        class_attrs: vec![],
        instance_attrs,
        methods,
        bases,
    })
}

#[test]
fn resolve_types() {
    let _ = env_logger::try_init();

    glob!("testdata/*.py", |path| {
        let source = fs::read_to_string(path).expect("Failed to read test file");
        let result = resolve_types_from_source(&source);
        assert_debug_snapshot!(result);
    });
}

#[cfg(test)]
mod unit_tests {
    use super::*;
    use super::super::typ::Basic;

    #[test]
    fn test_basic_str() {
        let result = resolve_types_from_source("x: str");
        assert_eq!(result.len(), 1);
        assert!(matches!(result.get("x"), Some(Type::Basic(Basic::Str))));
    }

    #[test]
    fn test_basic_int() {
        let result = resolve_types_from_source("x: int");
        assert_eq!(result.len(), 1);
        assert!(matches!(result.get("x"), Some(Type::Basic(Basic::Int))));
    }

    #[test]
    fn test_basic_float() {
        let result = resolve_types_from_source("x: float");
        assert_eq!(result.len(), 1);
        assert!(matches!(result.get("x"), Some(Type::Basic(Basic::Float))));
    }

    #[test]
    fn test_basic_bool() {
        let result = resolve_types_from_source("x: bool");
        assert_eq!(result.len(), 1);
        assert!(matches!(result.get("x"), Some(Type::Basic(Basic::Bool))));
    }

    #[test]
    fn test_basic_none() {
        let result = resolve_types_from_source("x: None");
        assert_eq!(result.len(), 1);
        assert!(matches!(result.get("x"), Some(Type::Basic(Basic::None))));
    }

    #[test]
    fn test_list_type() {
        let result = resolve_types_from_source("x: list[str]");
        assert_eq!(result.len(), 1);
        if let Some(Type::List(inner)) = result.get("x") {
            assert!(matches!(inner.as_ref(), Type::Basic(Basic::Str)));
        } else {
            panic!("Expected List type");
        }
    }

    #[test]
    fn test_dict_type() {
        let result = resolve_types_from_source("x: dict[str, int]");
        assert_eq!(result.len(), 1);
        if let Some(Type::Dict(key, val)) = result.get("x") {
            assert!(matches!(key.as_ref(), Type::Basic(Basic::Str)));
            assert!(matches!(val.as_ref(), Type::Basic(Basic::Int)));
        } else {
            panic!("Expected Dict type");
        }
    }

    #[test]
    fn test_optional_type() {
        let result = resolve_types_from_source("from typing import Optional\nx: Optional[str]");
        assert_eq!(result.len(), 1);
        if let Some(Type::Optional(inner)) = result.get("x") {
            assert!(matches!(inner.as_ref(), Type::Basic(Basic::Str)));
        } else {
            panic!("Expected Optional type");
        }
    }

    #[test]
    fn test_union_type_pep604() {
        let result = resolve_types_from_source("x: str | int");
        assert_eq!(result.len(), 1);
        if let Some(Type::Union(union)) = result.get("x") {
            assert_eq!(union.types.len(), 2);
        } else {
            panic!("Expected Union type");
        }
    }

    #[test]
    fn test_tuple_type() {
        let result = resolve_types_from_source("x: tuple[int, str]");
        assert_eq!(result.len(), 1);
        if let Some(Type::Tuple(types, variadic)) = result.get("x") {
            assert_eq!(types.len(), 2);
            assert!(!variadic);
        } else {
            panic!("Expected Tuple type");
        }
    }

    #[test]
    fn test_literal_type() {
        let result = resolve_types_from_source("from typing import Literal\nx: Literal[\"a\", \"b\"]");
        assert_eq!(result.len(), 1);
        if let Some(Type::Literal(lit)) = result.get("x") {
            assert_eq!(lit.values.len(), 2);
        } else {
            panic!("Expected Literal type");
        }
    }

    #[test]
    fn test_simple_function() {
        let result = resolve_types_from_source("def foo(x: int) -> str: pass");
        assert_eq!(result.len(), 1);
        if let Some(Type::Callable(func)) = result.get("foo") {
            assert_eq!(func.params.len(), 1);
            assert!(matches!(func.params[0].typ, Type::Basic(Basic::Int)));
            assert!(matches!(func.return_type.as_ref(), Type::Basic(Basic::Str)));
        } else {
            panic!("Expected Callable type");
        }
    }
}
