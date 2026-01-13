use crate::ast::{
    loader::{errors::ParseResult, fileset::Span},
    schema::object::ModuleObj,
};
use ruff_python_ast as ast;

use super::typ::{Basic, FuncType, Literal, Param, ParamKind, Type};

#[allow(dead_code)]
pub enum Decl<'py> {
    Type(TypeDecl<'py>),
    Func(FuncDecl<'py>),
}

pub struct TypeDecl<'py> {
    /// The source code range of the type declaration.
    pub span: Span,

    /// The name of the declaration.
    pub name: String,

    /// The underlying type of the declaration.
    pub typ: Type<'py>,

    /// Type parameters for generic types.
    pub type_params: Vec<DeclTypeParam>,
}

/// DeclTypeParam represents a type parameter in a type declaration.
/// For example A in "class MyClass[A](...): ..."
pub struct DeclTypeParam {
    /// The source code range of the type parameter.
    pub span: Span,

    /// The name of the type parameter.
    pub name: String,
}

/// FuncDecl represents a function declaration.
pub struct FuncDecl<'py> {
    /// The source code range of the function declaration.
    pub span: Span,

    /// The name of the function.
    pub name: String,

    /// The function's type signature (parameters and return type).
    pub typ: FuncType<'py>,

    /// Type parameters for generic functions (PEP 695).
    pub type_params: Vec<DeclTypeParam>,

    /// Whether the function is async.
    pub is_async: bool,
}

impl<'py> ModuleObj<'py> {
    /// Parses a function declaration from an AST node.
    ///
    /// This is modeled after the Go implementation in ParseFuncDecl,
    /// adapted for Python's syntax and semantics.
    pub fn parse_func_decl(&self, fd: &ast::StmtFunctionDef) -> ParseResult<FuncDecl<'py>> {
        let span = self.module.file.id.to_span(fd.range);
        let name = fd.name.to_string();
        let is_async = fd.is_async;

        // Compute type parameters from the function's type_params (PEP 695 style)
        let (type_params_in_scope, type_params) =
            compute_decl_type_params(fd.type_params.as_deref());

        // Parse the function's parameters
        let params = self.parse_parameters(&fd.parameters, &type_params_in_scope)?;

        // Parse the return type annotation if present
        let returns = if let Some(ref return_annotation) = fd.returns {
            Some(Box::new(self.parse_type_annotation(
                return_annotation,
                &type_params_in_scope,
            )?))
        } else {
            None
        };

        let typ = FuncType { params, returns };

        Ok(FuncDecl {
            span,
            name,
            typ,
            type_params,
            is_async,
        })
    }

    /// Parses function parameters from the AST Parameters node.
    fn parse_parameters(
        &self,
        params: &ast::Parameters,
        type_params_in_scope: &std::collections::HashMap<String, usize>,
    ) -> ParseResult<Vec<Param<'py>>> {
        let mut result = Vec::new();

        // Parse positional-only parameters (before /)
        for param in &params.posonlyargs {
            result.push(self.parse_param_with_default(
                param,
                ParamKind::PositionalOnly,
                type_params_in_scope,
            )?);
        }

        // Parse regular positional/keyword parameters
        for param in &params.args {
            result.push(self.parse_param_with_default(
                param,
                ParamKind::Regular,
                type_params_in_scope,
            )?);
        }

        // Parse *args (variadic positional)
        if let Some(ref vararg) = params.vararg {
            result.push(self.parse_param(
                vararg,
                ParamKind::VarPositional,
                false,
                type_params_in_scope,
            )?);
        }

        // Parse keyword-only parameters (after *)
        for param in &params.kwonlyargs {
            result.push(self.parse_param_with_default(
                param,
                ParamKind::KeywordOnly,
                type_params_in_scope,
            )?);
        }

        // Parse **kwargs (variadic keyword)
        if let Some(ref kwarg) = params.kwarg {
            result.push(self.parse_param(
                kwarg,
                ParamKind::VarKeyword,
                false,
                type_params_in_scope,
            )?);
        }

        Ok(result)
    }

    /// Parses a parameter that may have a default value.
    fn parse_param_with_default(
        &self,
        param: &ast::ParameterWithDefault,
        kind: ParamKind,
        type_params_in_scope: &std::collections::HashMap<String, usize>,
    ) -> ParseResult<Param<'py>> {
        let has_default = param.default.is_some();
        self.parse_param(&param.parameter, kind, has_default, type_params_in_scope)
    }

    /// Parses a single parameter.
    fn parse_param(
        &self,
        param: &ast::Parameter,
        kind: ParamKind,
        has_default: bool,
        type_params_in_scope: &std::collections::HashMap<String, usize>,
    ) -> ParseResult<Param<'py>> {
        let name = param.name.to_string();
        let typ = if let Some(ref annotation) = param.annotation {
            Some(self.parse_type_annotation(annotation, type_params_in_scope)?)
        } else {
            None
        };

        Ok(Param {
            name,
            typ,
            has_default,
            kind,
        })
    }

    /// Parses a type annotation expression.
    fn parse_type_annotation(
        &self,
        expr: &ast::Expr,
        type_params_in_scope: &std::collections::HashMap<String, usize>,
    ) -> ParseResult<Type<'py>> {
        use ast::Expr;

        Ok(match expr {
            Expr::StringLiteral(x) => Type::Literal(Literal::String(x.value.to_string())),
            Expr::NumberLiteral(x) => match &x.value {
                ast::Number::Int(i) => match i.as_i64() {
                    Some(i) => Type::Literal(Literal::Int(i)),
                    None => Type::Literal(Literal::BigInt(i.to_string())),
                },
                ast::Number::Float(f) => Type::Literal(Literal::Float(*f)),
                ast::Number::Complex { .. } => {
                    return Err(self.parse_err(
                        expr,
                        "complex literals are not supported in type annotations",
                    ))
                }
            },
            Expr::BooleanLiteral(x) => Type::Literal(Literal::Boolean(x.value)),
            Expr::NoneLiteral(_) => Type::Basic(Basic::None),
            Expr::Name(name) => match name.id.as_str() {
                "int" => Type::Basic(Basic::Int),
                "str" => Type::Basic(Basic::Str),
                "float" => Type::Basic(Basic::Float),
                "bool" => Type::Basic(Basic::Bool),
                "bytes" => Type::Basic(Basic::Bytes),
                "Any" => Type::Basic(Basic::Any),
                "None" => Type::Basic(Basic::None),
                other => {
                    // Check if this is a reference to a type parameter
                    if let Some(&index) = type_params_in_scope.get(other) {
                        return Ok(Type::TypeParamRefType(index));
                    }
                    // TODO: Handle named types (class references)
                    return Err(self.parse_err(expr, format!("unknown type '{}'", other)));
                }
            },
            Expr::Subscript(_) => {
                // TODO: Handle generic types like List[int], Dict[str, int], etc.
                return Err(self.parse_err(expr, "subscript types not yet implemented"));
            }
            Expr::Tuple(_) => {
                // TODO: Handle tuple types
                return Err(self.parse_err(expr, "tuple types not yet implemented"));
            }
            Expr::BinOp(binop) if matches!(binop.op, ast::Operator::BitOr) => {
                // Handle union types with | syntax (PEP 604)
                // TODO: Implement union type parsing
                return Err(self.parse_err(expr, "union types (X | Y) not yet implemented"));
            }
            _ => return Err(self.parse_err(expr, "unexpected expression in type annotation")),
        })
    }

    /// Creates a parse error at the given expression's location.
    fn parse_err<R: ruff_text_size::Ranged>(
        &self,
        r: R,
        msg: impl Into<String>,
    ) -> crate::ast::loader::errors::ParseError {
        crate::ast::loader::errors::ParseError::Parse {
            span: self.module.file.id.to_span(r.range()),
            message: msg.into(),
        }
    }
}

/// Computes the type parameter placeholders for a declaration.
///
/// For example, given the type parameters in:
///     def foo[A, B](x: A) -> B: ...
///
/// It returns:
///     name_map = {"A": 0, "B": 1}
///     params = [{name: "A", span: ...}, {name: "B", span: ...}]
fn compute_decl_type_params(
    type_params: Option<&ast::TypeParams>,
) -> (std::collections::HashMap<String, usize>, Vec<DeclTypeParam>) {
    use std::collections::HashMap;

    let Some(type_params) = type_params else {
        return (HashMap::new(), Vec::new());
    };

    if type_params.is_empty() {
        return (HashMap::new(), Vec::new());
    }

    let mut name_map = HashMap::new();
    let mut params = Vec::new();

    for (idx, type_param) in type_params.iter().enumerate() {
        match type_param {
            ast::TypeParam::TypeVar(tv) => {
                let name = tv.name.to_string();
                name_map.insert(name.clone(), idx);
                params.push(DeclTypeParam {
                    span: Span::default(), // TODO: Get proper span from file context
                    name,
                });
            }
            ast::TypeParam::TypeVarTuple(tv) => {
                let name = tv.name.to_string();
                name_map.insert(name.clone(), idx);
                params.push(DeclTypeParam {
                    span: Span::default(),
                    name,
                });
            }
            ast::TypeParam::ParamSpec(ps) => {
                let name = ps.name.to_string();
                name_map.insert(name.clone(), idx);
                params.push(DeclTypeParam {
                    span: Span::default(),
                    name,
                });
            }
        }
    }

    (name_map, params)
}
