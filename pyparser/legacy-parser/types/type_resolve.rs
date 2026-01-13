//! Type resolution from Python AST expressions.
//!
//! This module provides the `TypeChecker` and `Ctx` types for resolving
//! Python type annotations (AST expressions) into our `Type` representation.
//!
//! # Architecture
//!
//! Type resolution follows a context-based pattern:
//!
//! 1. `TypeChecker` is the entry point, holding global state
//! 2. `Ctx` is a resolution context created for each resolution operation
//! 3. Context can be nested for scoped resolution (e.g., inside generic classes)
//!
//! # Example
//!
//! ```ignore
//! let state = ResolveState::new();
//! let checker = TypeChecker::new(&state);
//!
//! // Resolve a type annotation expression
//! let typ = checker.resolve_type(module_id, &type_expr);
//! ```

use ruff_python_ast as ast;
use ruff_text_size::TextRange;

use super::object::{
    CheckState, Class, Func, ModuleId, Object, ObjectKind, ResolveState, TypeAlias, TypeParamDef,
    TypeParamKind, Var,
};
use super::resolved::Resolved;
use super::typ::*;
use super::validation::{Expr as ValidationExpr, Rule, N};

/// Type checker entry point.
///
/// The TypeChecker holds a reference to the global ResolveState and provides
/// methods for resolving types from AST expressions.
pub struct TypeChecker<'a> {
    state: &'a ResolveState,
}

impl<'a> TypeChecker<'a> {
    /// Create a new TypeChecker.
    pub fn new(state: &'a ResolveState) -> Self {
        TypeChecker { state }
    }

    /// Resolve a type annotation expression.
    pub fn resolve_type(&self, module_id: ModuleId, expr: &ast::Expr) -> Type {
        let ctx = Ctx::new(self.state, module_id);
        ctx.resolve_expr(expr)
    }

    /// Resolve a type with type parameter context.
    pub fn resolve_type_with_params(
        &self,
        module_id: ModuleId,
        expr: &ast::Expr,
        type_params: &[TypeParamDef],
    ) -> Type {
        let ctx = Ctx::new(self.state, module_id).with_type_params(type_params);
        ctx.resolve_expr(expr)
    }

    /// Get the concrete type, resolving all named references.
    pub fn concrete(&self, module_id: ModuleId, typ: &Type) -> Type {
        let ctx = Ctx::new(self.state, module_id);
        ctx.concrete(typ).into_owned()
    }

    /// Get the underlying type, unwrapping Optional/Validated/Named.
    pub fn underlying(&self, module_id: ModuleId, typ: &Type) -> Type {
        let ctx = Ctx::new(self.state, module_id);
        ctx.underlying(typ)
    }

    /// Resolve the type of an Object.
    pub fn resolve_obj_type(&self, obj: &Object) -> Type {
        // Check if already resolved
        if let Some(typ) = obj.resolved_type() {
            return typ;
        }

        // Check for cycles
        if obj.is_resolving() {
            // Return a named reference to break the cycle
            return Type::Named(Named {
                obj: self.state.get_object(obj.id).unwrap(),
                type_arguments: Vec::new(),
            });
        }

        // Mark as in progress
        *obj.state.borrow_mut() = CheckState::InProgress;

        // Resolve based on kind
        let typ = match &obj.kind {
            ObjectKind::TypeAlias(alias) => self.resolve_type_alias(obj, alias),
            ObjectKind::Class(class) => self.resolve_class(obj, class),
            ObjectKind::Func(func) => self.resolve_func(obj, func),
            ObjectKind::Var(var) => self.resolve_var(obj, var),
            ObjectKind::Enum(enum_def) => self.resolve_enum(obj, enum_def),
            ObjectKind::TypeVar(tv_def) => self.resolve_typevar_def(obj, tv_def),
            ObjectKind::Protocol(proto) => self.resolve_protocol(obj, proto),
            ObjectKind::TypedDict(td) => self.resolve_typed_dict(obj, td),
            ObjectKind::Module(_) | ObjectKind::Namespace(_) => Type::Any,
        };

        // Cache the result
        *obj.state.borrow_mut() = CheckState::Completed(typ.clone());

        typ
    }

    fn resolve_type_alias(&self, obj: &Object, alias: &TypeAlias) -> Type {
        if let Some(ref value) = alias.value {
            let ctx = Ctx::new(self.state, obj.module_id).with_type_params(&alias.type_params);
            ctx.resolve_expr(value)
        } else {
            Type::Any
        }
    }

    fn resolve_class(&self, obj: &Object, class: &Class) -> Type {
        let obj_rc = self.state.get_object(obj.id).unwrap();
        let ctx = Ctx::new(self.state, obj.module_id).with_type_params(&class.type_params);

        // Resolve base classes
        let bases: Vec<Type> = class.bases.iter().map(|b| ctx.resolve_expr(b)).collect();

        // TODO: Extract class and instance attributes from body
        // This would require analyzing the class body statements

        Type::Class(ClassType {
            obj: obj_rc,
            type_arguments: Vec::new(),
            class_attrs: Vec::new(),
            instance_attrs: Vec::new(),
            methods: Vec::new(),
            bases,
        })
    }

    fn resolve_func(&self, obj: &Object, func: &Func) -> Type {
        let ctx = Ctx::new(self.state, obj.module_id).with_type_params(&func.type_params);

        // Resolve parameter types
        let params = resolve_parameters(&ctx, &func.params);

        // Resolve return type
        let return_type = if let Some(ref returns) = func.returns {
            ctx.resolve_expr(returns)
        } else {
            Type::Any
        };

        // Convert type params
        let type_params = if func.type_params.is_empty() {
            None
        } else {
            Some(
                func.type_params
                    .iter()
                    .enumerate()
                    .map(|(idx, p)| TypeParam {
                        name: p.name.clone(),
                        idx,
                        constraint: p.bound.as_ref().map(|b| Box::new(ctx.resolve_expr(b))),
                        variance: Variance::Invariant,
                    })
                    .collect(),
            )
        };

        Type::Callable(FunctionType {
            params,
            return_type: Box::new(return_type),
            type_params,
            is_async: func.is_async,
        })
    }

    fn resolve_var(&self, obj: &Object, var: &Var) -> Type {
        let ctx = Ctx::new(self.state, obj.module_id);

        if let Some(ref annotation) = var.annotation {
            ctx.resolve_expr(annotation)
        } else {
            // Try to infer from value
            // For now, return Any
            Type::Any
        }
    }

    fn resolve_enum(&self, obj: &Object, enum_def: &super::object::Enum) -> Type {
        let obj_rc = self.state.get_object(obj.id).unwrap();

        let members: Vec<EnumMember> = enum_def
            .members
            .iter()
            .map(|m| EnumMember {
                name: m.name.clone(),
                value: None, // TODO: Resolve enum member values
            })
            .collect();

        Type::Enum(EnumType {
            obj: obj_rc,
            members,
        })
    }

    fn resolve_typevar_def(&self, obj: &Object, tv_def: &super::object::TypeVarDef) -> Type {
        let ctx = Ctx::new(self.state, obj.module_id);

        let constraints: Vec<Type> = tv_def
            .constraints
            .iter()
            .map(|c| ctx.resolve_expr(c))
            .collect();

        let bound = tv_def.bound.as_ref().map(|b| Box::new(ctx.resolve_expr(b)));

        let variance = if tv_def.covariant {
            Variance::Covariant
        } else if tv_def.contravariant {
            Variance::Contravariant
        } else {
            Variance::Invariant
        };

        Type::TypeVar(TypeVar {
            name: tv_def.name.clone(),
            constraints,
            bound,
            variance,
        })
    }

    fn resolve_protocol(&self, obj: &Object, proto: &super::object::Protocol) -> Type {
        let _obj_rc = self.state.get_object(obj.id).unwrap();
        let ctx = Ctx::new(self.state, obj.module_id).with_type_params(&proto.type_params);

        // Resolve protocol members as interface fields
        let fields: Vec<InterfaceField> = proto
            .members
            .iter()
            .filter_map(|m| {
                let typ = match &m.kind {
                    super::object::ProtocolMemberKind::Attribute => {
                        m.annotation.as_ref().map(|a| ctx.resolve_expr(a))
                    }
                    super::object::ProtocolMemberKind::Method(params, returns) => {
                        let func_params = resolve_parameters(&ctx, params);
                        let return_type = returns
                            .as_ref()
                            .map(|r| ctx.resolve_expr(r))
                            .unwrap_or(Type::Any);
                        Some(Type::Callable(FunctionType {
                            params: func_params,
                            return_type: Box::new(return_type),
                            type_params: None,
                            is_async: false,
                        }))
                    }
                    super::object::ProtocolMemberKind::Property => {
                        m.annotation.as_ref().map(|a| ctx.resolve_expr(a))
                    }
                }?;

                Some(InterfaceField {
                    range: TextRange::default(),
                    name: FieldName::String(m.name.clone()),
                    optional: false,
                    readonly: matches!(m.kind, super::object::ProtocolMemberKind::Property),
                    typ,
                    doc: None,
                })
            })
            .collect();

        Type::Interface(Interface {
            fields,
            index_signature: None,
            total: true,
        })
    }

    fn resolve_typed_dict(&self, obj: &Object, td: &super::object::TypedDictDef) -> Type {
        let ctx = Ctx::new(self.state, obj.module_id).with_type_params(&td.type_params);

        let fields: Vec<InterfaceField> = td
            .fields
            .iter()
            .map(|f| {
                let typ = ctx.resolve_expr(&f.annotation);
                InterfaceField {
                    range: f.range,
                    name: FieldName::String(f.name.clone()),
                    optional: !f.required.unwrap_or(td.total),
                    readonly: false,
                    typ,
                    doc: None,
                }
            })
            .collect();

        Type::Interface(Interface {
            fields,
            index_signature: None,
            total: td.total,
        })
    }
}

/// Resolution context for a specific scope.
///
/// The context tracks type parameters, module scope, and provides methods
/// for resolving type expressions.
pub struct Ctx<'a> {
    state: &'a ResolveState,
    module_id: ModuleId,
    /// Type parameters in scope (from enclosing generic class/function).
    type_params: &'a [TypeParamDef],
    /// Type arguments to substitute for type parameters.
    type_args: &'a [Type],
}

impl<'a> Ctx<'a> {
    /// Create a new resolution context.
    pub fn new(state: &'a ResolveState, module_id: ModuleId) -> Self {
        Ctx {
            state,
            module_id,
            type_params: &[],
            type_args: &[],
        }
    }

    /// Create a new context with type parameters.
    pub fn with_type_params(self, params: &'a [TypeParamDef]) -> Self {
        Ctx {
            state: self.state,
            module_id: self.module_id,
            type_params: params,
            type_args: self.type_args,
        }
    }

    /// Create a new context with type arguments (borrows from self).
    pub fn with_type_args_ref<'b>(&'b self, args: &'b [Type]) -> Ctx<'b>
    where
        'a: 'b,
    {
        Ctx {
            state: self.state,
            module_id: self.module_id,
            type_params: self.type_params,
            type_args: args,
        }
    }

    /// Create a new context with type arguments (consumes self).
    pub fn with_type_args(self, args: &'a [Type]) -> Self {
        Ctx {
            state: self.state,
            module_id: self.module_id,
            type_params: self.type_params,
            type_args: args,
        }
    }

    /// Resolve a type annotation expression to a Type.
    pub fn resolve_expr(&self, expr: &ast::Expr) -> Type {
        match expr {
            // Simple name: int, str, MyClass, T, etc.
            ast::Expr::Name(name) => self.resolve_name(&name),

            // Subscript: List[int], Dict[str, int], Optional[str], etc.
            ast::Expr::Subscript(sub) => self.resolve_subscript(sub),

            // Attribute: typing.List, collections.abc.Mapping, etc.
            ast::Expr::Attribute(attr) => self.resolve_attribute(attr),

            // Binary or: str | int | None (PEP 604 union)
            ast::Expr::BinOp(binop) if matches!(binop.op, ast::Operator::BitOr) => {
                self.resolve_union_binop(binop)
            }

            // Tuple: (int, str) in some contexts
            ast::Expr::Tuple(tuple) => self.resolve_tuple_expr(tuple),

            // None literal
            ast::Expr::NoneLiteral(_) => Type::Basic(Basic::None),

            // String literal (forward reference)
            ast::Expr::StringLiteral(_s) => {
                // TODO: Parse string as type expression (forward reference)
                // For now, treat as Any
                Type::Any
            }

            // Ellipsis (used in Callable[..., ReturnType])
            ast::Expr::EllipsisLiteral(_) => Type::Basic(Basic::Ellipsis),

            // Call expression: Annotated[T, Field(...)], etc.
            ast::Expr::Call(call) => self.resolve_call(call),

            // List literal (shouldn't appear in annotations, but handle gracefully)
            ast::Expr::List(_) => Type::Any,

            // Starred expression (shouldn't appear at top level)
            ast::Expr::Starred(starred) => self.resolve_expr(&starred.value),

            // Unary not (rare in type expressions)
            _ => Type::Any,
        }
    }

    /// Resolve a simple name to a type.
    fn resolve_name(&self, name: &ast::ExprName) -> Type {
        let name_str: &str = &name.id;
        // Check builtin types first
        if let Some(basic) = self.builtin_type(name_str) {
            return Type::Basic(basic);
        }

        // Check type parameters
        for (idx, param) in self.type_params.iter().enumerate() {
            if param.name == name_str {
                let constraint = param.bound.as_ref().map(|b| Box::new(self.resolve_expr(b)));
                return Type::Generic(Generic::TypeParam(TypeParam {
                    name: name_str.to_string(),
                    idx,
                    constraint,
                    variance: match param.kind {
                        TypeParamKind::Type => Variance::Invariant,
                        TypeParamKind::TypeWithVariance(v) => v,
                        _ => Variance::Invariant,
                    },
                }));
            }
        }

        // Check module scope
        if let Some(module_data) = self.state.get_module(self.module_id) {
            if let Some(obj) = module_data.top_level.get(name_str) {
                return Type::Named(Named {
                    obj: obj.clone(),
                    type_arguments: Vec::new(),
                });
            }
        }

        // Unknown type. Log an error.
        Type::Any
    }

    /// Map a name to a builtin Basic type.
    fn builtin_type(&self, name: &str) -> Option<Basic> {
        match name {
            "str" => Some(Basic::Str),
            "int" => Some(Basic::Int),
            "float" => Some(Basic::Float),
            "bool" => Some(Basic::Bool),
            "bytes" => Some(Basic::Bytes),
            "bytearray" => Some(Basic::ByteArray),
            "None" | "NoneType" => Some(Basic::None),
            "object" => Some(Basic::Object),
            "complex" => Some(Basic::Complex),
            "type" => Some(Basic::Type),
            _ => None,
        }
    }

    /// Resolve a subscript expression: List[int], Dict[str, int], etc.
    fn resolve_subscript(&self, sub: &ast::ExprSubscript) -> Type {
        let base_name = self.get_type_name(&sub.value);

        match base_name.as_deref() {
            // typing.List or list
            Some("List" | "list") => {
                let inner = self.resolve_subscript_args_single(&sub.slice);
                Type::List(Box::new(inner))
            }

            // typing.Set or set
            Some("Set" | "set") => {
                let inner = self.resolve_subscript_args_single(&sub.slice);
                Type::Set(Box::new(inner))
            }

            // typing.FrozenSet or frozenset
            Some("FrozenSet" | "frozenset") => {
                let inner = self.resolve_subscript_args_single(&sub.slice);
                Type::FrozenSet(Box::new(inner))
            }

            // typing.Dict or dict
            Some("Dict" | "dict") => {
                let (key, value) = self.resolve_subscript_args_pair(&sub.slice);
                Type::Dict(Box::new(key), Box::new(value))
            }

            // typing.Tuple or tuple
            Some("Tuple" | "tuple") => self.resolve_tuple_subscript(&sub.slice),

            // typing.Optional
            Some("Optional") => {
                let inner = self.resolve_subscript_args_single(&sub.slice);
                Type::Optional(Box::new(inner))
            }

            // typing.Union
            Some("Union") => {
                let types = self.resolve_subscript_args_list(&sub.slice);
                Union::new(types).simplify()
            }

            // typing.Literal
            Some("Literal") => self.resolve_literal_subscript(&sub.slice),

            // typing.Callable
            Some("Callable") => self.resolve_callable_subscript(&sub.slice),

            // typing.Annotated
            Some("Annotated") => self.resolve_annotated_subscript(&sub.slice),

            // typing.ClassVar
            Some("ClassVar") => {
                // ClassVar[T] - just return T for type checking purposes
                self.resolve_subscript_args_single(&sub.slice)
            }

            // typing.Final
            Some("Final") => {
                // Final[T] - just return T for type checking purposes
                self.resolve_subscript_args_single(&sub.slice)
            }

            // typing.Type or type
            Some("Type" | "type") => {
                // Type[T] represents the class T itself
                // For now, simplify to Any
                Type::Any
            }

            // Generic named type with type arguments
            _ => {
                let base = self.resolve_expr(&sub.value);
                let args = self.resolve_subscript_args_list(&sub.slice);

                // If base is a Named type, add type arguments
                if let Type::Named(mut named) = base {
                    named.type_arguments = args;
                    Type::Named(named)
                } else {
                    base
                }
            }
        }
    }

    /// Get the name of a type expression (for matching known types).
    fn get_type_name(&self, expr: &ast::Expr) -> Option<String> {
        match expr {
            ast::Expr::Name(name) => Some(name.id.to_string()),
            ast::Expr::Attribute(attr) => Some(attr.attr.to_string()),
            _ => None,
        }
    }

    /// Resolve a subscript with a single type argument.
    fn resolve_subscript_args_single(&self, slice: &ast::Expr) -> Type {
        self.resolve_expr(slice)
    }

    /// Resolve a subscript with two type arguments (key, value).
    fn resolve_subscript_args_pair(&self, slice: &ast::Expr) -> (Type, Type) {
        if let ast::Expr::Tuple(tuple) = slice {
            if tuple.elts.len() >= 2 {
                let key = self.resolve_expr(&tuple.elts[0]);
                let value = self.resolve_expr(&tuple.elts[1]);
                return (key, value);
            }
        }
        (Type::Any, Type::Any)
    }

    /// Resolve a subscript with multiple type arguments.
    fn resolve_subscript_args_list(&self, slice: &ast::Expr) -> Vec<Type> {
        if let ast::Expr::Tuple(tuple) = slice {
            tuple.elts.iter().map(|e| self.resolve_expr(e)).collect()
        } else {
            vec![self.resolve_expr(slice)]
        }
    }

    /// Resolve Tuple[...] subscript.
    fn resolve_tuple_subscript(&self, slice: &ast::Expr) -> Type {
        if let ast::Expr::Tuple(tuple) = slice {
            // Check for Tuple[T, ...] (variable length)
            let elts = &tuple.elts;
            if elts.len() == 2 {
                if let ast::Expr::EllipsisLiteral(_) = &elts[1] {
                    let inner = self.resolve_expr(&elts[0]);
                    return Type::Tuple(vec![inner], true);
                }
            }
            // Fixed-length tuple
            let types: Vec<Type> = elts.iter().map(|e| self.resolve_expr(e)).collect();
            Type::Tuple(types, false)
        } else {
            // Single-element tuple
            Type::Tuple(vec![self.resolve_expr(slice)], false)
        }
    }

    /// Resolve Literal[...] subscript.
    fn resolve_literal_subscript(&self, slice: &ast::Expr) -> Type {
        let values = if let ast::Expr::Tuple(tuple) = slice {
            tuple
                .elts
                .iter()
                .filter_map(|e| self.expr_to_literal(e))
                .collect()
        } else {
            self.expr_to_literal(slice)
                .map(|l| vec![l])
                .unwrap_or_default()
        };

        Type::Literal(LiteralType { values })
    }

    /// Convert an expression to a Literal value.
    fn expr_to_literal(&self, expr: &ast::Expr) -> Option<Literal> {
        match expr {
            ast::Expr::StringLiteral(s) => Some(Literal::String(s.value.to_string())),
            ast::Expr::BytesLiteral(b) => Some(Literal::Bytes(
                b.value.iter().flat_map(|b| b.iter().copied()).collect(),
            )),
            ast::Expr::NumberLiteral(n) => match &n.value {
                ast::Number::Int(i) => i.as_i64().map(Literal::Int),
                ast::Number::Float(f) => Some(Literal::Float(*f)),
                ast::Number::Complex { .. } => None,
            },
            ast::Expr::BooleanLiteral(b) => Some(Literal::Bool(b.value)),
            ast::Expr::NoneLiteral(_) => Some(Literal::None),
            _ => None,
        }
    }

    /// Resolve Callable[[Args], Return] subscript.
    fn resolve_callable_subscript(&self, slice: &ast::Expr) -> Type {
        if let ast::Expr::Tuple(tuple) = slice {
            if tuple.elts.len() == 2 {
                let params = self.resolve_callable_params(&tuple.elts[0]);
                let return_type = self.resolve_expr(&tuple.elts[1]);

                return Type::Callable(FunctionType {
                    params,
                    return_type: Box::new(return_type),
                    type_params: None,
                    is_async: false,
                });
            }
        }
        Type::Any
    }

    /// Resolve Callable parameter types.
    fn resolve_callable_params(&self, expr: &ast::Expr) -> Vec<FunctionParam> {
        match expr {
            // Callable[..., R] - any args
            ast::Expr::EllipsisLiteral(_) => {
                vec![FunctionParam {
                    name: None,
                    typ: Type::Any,
                    optional: false,
                    variadic: true,
                    keyword_variadic: false,
                    kind: ParamKind::Regular,
                }]
            }
            // Callable[[A, B, C], R] - specific args
            ast::Expr::List(list) => list
                .elts
                .iter()
                .map(|e| FunctionParam {
                    name: None,
                    typ: self.resolve_expr(e),
                    optional: false,
                    variadic: false,
                    keyword_variadic: false,
                    kind: ParamKind::Regular,
                })
                .collect(),
            _ => Vec::new(),
        }
    }

    /// Resolve Annotated[T, metadata...] subscript.
    fn resolve_annotated_subscript(&self, slice: &ast::Expr) -> Type {
        if let ast::Expr::Tuple(tuple) = slice {
            if !tuple.elts.is_empty() {
                let inner = self.resolve_expr(&tuple.elts[0]);
                let metadata: Vec<AnnotationMetadata> = tuple.elts[1..]
                    .iter()
                    .filter_map(|e| self.resolve_annotation_metadata(e))
                    .collect();

                if metadata.is_empty() {
                    return inner;
                }

                return Type::Annotated(Annotated {
                    typ: Box::new(inner),
                    metadata,
                });
            }
        }
        Type::Any
    }

    /// Resolve annotation metadata (Field(...), validators, etc.).
    fn resolve_annotation_metadata(&self, expr: &ast::Expr) -> Option<AnnotationMetadata> {
        match expr {
            ast::Expr::Call(call) => {
                let name = self.get_type_name(&call.func)?;
                match name.as_str() {
                    "Field" => self.resolve_field_metadata(call),
                    "Gt" | "Ge" | "Lt" | "Le" => self.resolve_numeric_constraint(call, &name),
                    "MinLen" | "MaxLen" => self.resolve_length_constraint(call, &name),
                    _ => Some(AnnotationMetadata::Custom(name)),
                }
            }
            ast::Expr::StringLiteral(s) => Some(AnnotationMetadata::Doc(s.value.to_string())),
            _ => None,
        }
    }

    /// Resolve pydantic Field(...) metadata.
    fn resolve_field_metadata(&self, call: &ast::ExprCall) -> Option<AnnotationMetadata> {
        let mut config = FieldConfig::default();
        let mut validation_rules = Vec::new();

        for keyword in &call.arguments.keywords {
            if let Some(ref arg) = keyword.arg {
                let arg_str = arg.as_str();
                match arg_str {
                    "default" => {
                        config.default = Some(format!("{:?}", keyword.value));
                    }
                    "default_factory" => {
                        config.default_factory = Some(format!("{:?}", keyword.value));
                    }
                    "alias" => {
                        if let ast::Expr::StringLiteral(s) = &keyword.value {
                            config.alias = Some(s.value.to_string());
                        }
                    }
                    "title" => {
                        if let ast::Expr::StringLiteral(s) = &keyword.value {
                            config.title = Some(s.value.to_string());
                        }
                    }
                    "description" => {
                        if let ast::Expr::StringLiteral(s) = &keyword.value {
                            config.description = Some(s.value.to_string());
                        }
                    }
                    "min_length" => {
                        if let Some(n) = self.extract_int(&keyword.value) {
                            validation_rules.push(Rule::MinLen(n as u64));
                        }
                    }
                    "max_length" => {
                        if let Some(n) = self.extract_int(&keyword.value) {
                            validation_rules.push(Rule::MaxLen(n as u64));
                        }
                    }
                    "gt" => {
                        if let Some(n) = self.extract_number(&keyword.value) {
                            validation_rules.push(Rule::Gt(n));
                        }
                    }
                    "ge" => {
                        if let Some(n) = self.extract_number(&keyword.value) {
                            validation_rules.push(Rule::Ge(n));
                        }
                    }
                    "lt" => {
                        if let Some(n) = self.extract_number(&keyword.value) {
                            validation_rules.push(Rule::Lt(n));
                        }
                    }
                    "le" => {
                        if let Some(n) = self.extract_number(&keyword.value) {
                            validation_rules.push(Rule::Le(n));
                        }
                    }
                    "pattern" | "regex" => {
                        if let ast::Expr::StringLiteral(s) = &keyword.value {
                            validation_rules.push(Rule::Pattern(s.value.to_string()));
                        }
                    }
                    _ => {}
                }
            }
        }

        if !validation_rules.is_empty() {
            let expr = validation_rules
                .into_iter()
                .map(ValidationExpr::Rule)
                .reduce(|a, b| a.and(b))
                .unwrap();
            return Some(AnnotationMetadata::Validation(expr));
        }

        Some(AnnotationMetadata::FieldConfig(config))
    }

    /// Resolve numeric constraint metadata (Gt, Ge, Lt, Le).
    fn resolve_numeric_constraint(
        &self,
        call: &ast::ExprCall,
        name: &str,
    ) -> Option<AnnotationMetadata> {
        let value = call.arguments.args.first()?;
        let n = self.extract_number(value)?;

        let rule = match name {
            "Gt" => Rule::Gt(n),
            "Ge" => Rule::Ge(n),
            "Lt" => Rule::Lt(n),
            "Le" => Rule::Le(n),
            _ => return None,
        };

        Some(AnnotationMetadata::Validation(ValidationExpr::Rule(rule)))
    }

    /// Resolve length constraint metadata (MinLen, MaxLen).
    fn resolve_length_constraint(
        &self,
        call: &ast::ExprCall,
        name: &str,
    ) -> Option<AnnotationMetadata> {
        let value = call.arguments.args.first()?;
        let n = self.extract_int(value)? as u64;

        let rule = match name {
            "MinLen" => Rule::MinLen(n),
            "MaxLen" => Rule::MaxLen(n),
            _ => return None,
        };

        Some(AnnotationMetadata::Validation(ValidationExpr::Rule(rule)))
    }

    /// Extract an integer from an expression.
    fn extract_int(&self, expr: &ast::Expr) -> Option<i64> {
        if let ast::Expr::NumberLiteral(n) = expr {
            if let ast::Number::Int(i) = &n.value {
                return i.as_i64();
            }
        }
        None
    }

    /// Extract a number (int or float) from an expression.
    fn extract_number(&self, expr: &ast::Expr) -> Option<N> {
        if let ast::Expr::NumberLiteral(n) = expr {
            match &n.value {
                ast::Number::Int(i) => i.as_i64().map(N::Int),
                ast::Number::Float(f) => Some(N::Float(*f)),
                _ => None,
            }
        } else {
            None
        }
    }

    /// Resolve an attribute access: typing.List, etc.
    fn resolve_attribute(&self, attr: &ast::ExprAttribute) -> Type {
        // For now, just look at the final attribute name
        let val = self.resolve_expr(&attr.value);

        self.resolve_name(&attr.attr)
    }

    /// Resolve a union using | operator (PEP 604).
    fn resolve_union_binop(&self, binop: &ast::ExprBinOp) -> Type {
        let left = self.resolve_expr(&binop.left);
        let right = self.resolve_expr(&binop.right);
        left.union_merge(right)
    }

    /// Resolve a tuple expression in type context.
    fn resolve_tuple_expr(&self, tuple: &ast::ExprTuple) -> Type {
        let types: Vec<Type> = tuple.elts.iter().map(|e| self.resolve_expr(e)).collect();
        Type::Tuple(types, false)
    }

    /// Resolve a call expression in type context.
    fn resolve_call(&self, call: &ast::ExprCall) -> Type {
        // Calls in type context are usually TypeVar() or similar
        let name = self.get_type_name(&call.func);

        match name.as_deref() {
            Some("TypeVar") => self.resolve_typevar_call(call),
            _ => Type::Any,
        }
    }

    /// Resolve TypeVar(...) call.
    fn resolve_typevar_call(&self, call: &ast::ExprCall) -> Type {
        // TypeVar('T', bound=SomeType, covariant=True, etc.)
        let name = call
            .arguments
            .args
            .first()
            .and_then(|e| {
                if let ast::Expr::StringLiteral(s) = e {
                    Some(s.value.to_string())
                } else {
                    None
                }
            })
            .unwrap_or_else(|| "T".to_string());

        let mut constraints = Vec::new();
        let mut bound = None;
        let mut variance = Variance::Invariant;

        // Parse additional positional args as constraints
        for arg in call.arguments.args.iter().skip(1) {
            constraints.push(self.resolve_expr(arg));
        }

        // Parse keyword args
        for keyword in &call.arguments.keywords {
            if let Some(ref arg) = keyword.arg {
                match arg.as_str() {
                    "bound" => {
                        bound = Some(Box::new(self.resolve_expr(&keyword.value)));
                    }
                    "covariant" => {
                        if let ast::Expr::BooleanLiteral(b) = &keyword.value {
                            if b.value {
                                variance = Variance::Covariant;
                            }
                        }
                    }
                    "contravariant" => {
                        if let ast::Expr::BooleanLiteral(b) = &keyword.value {
                            if b.value {
                                variance = Variance::Contravariant;
                            }
                        }
                    }
                    _ => {}
                }
            }
        }

        Type::TypeVar(TypeVar {
            name,
            constraints,
            bound,
            variance,
        })
    }

    /// Get the concrete type, resolving all Named references.
    pub fn concrete<'b>(&self, typ: &'b Type) -> Resolved<'b, Type> {
        match typ {
            Type::Named(named) => {
                // Resolve the named type
                if let Some(resolved) = named.obj.resolved_type() {
                    // Apply type arguments
                    if !named.type_arguments.is_empty() {
                        let ctx = self.with_type_args_ref(&named.type_arguments);
                        Resolved::New(ctx.concrete(&resolved).into_owned())
                    } else {
                        Resolved::New(resolved)
                    }
                } else {
                    Resolved::Same(typ)
                }
            }
            Type::Generic(Generic::TypeParam(param)) => {
                // Substitute type argument if available
                if param.idx < self.type_args.len() {
                    Resolved::New(self.type_args[param.idx].clone())
                } else {
                    Resolved::Same(typ)
                }
            }
            Type::List(inner) => {
                let resolved = self.concrete(inner);
                if resolved.is_changed() {
                    Resolved::New(Type::List(Box::new(resolved.into_owned())))
                } else {
                    Resolved::Same(typ)
                }
            }
            Type::Set(inner) => {
                let resolved = self.concrete(inner);
                if resolved.is_changed() {
                    Resolved::New(Type::Set(Box::new(resolved.into_owned())))
                } else {
                    Resolved::Same(typ)
                }
            }
            Type::Dict(key, value) => {
                let key_resolved = self.concrete(key);
                let value_resolved = self.concrete(value);
                if key_resolved.is_changed() || value_resolved.is_changed() {
                    Resolved::New(Type::Dict(
                        Box::new(key_resolved.into_owned()),
                        Box::new(value_resolved.into_owned()),
                    ))
                } else {
                    Resolved::Same(typ)
                }
            }
            Type::Optional(inner) => {
                let resolved = self.concrete(inner);
                if resolved.is_changed() {
                    Resolved::New(Type::Optional(Box::new(resolved.into_owned())))
                } else {
                    Resolved::Same(typ)
                }
            }
            Type::Union(union) => {
                let mut changed = false;
                let types: Vec<Type> = union
                    .types
                    .iter()
                    .map(|t| {
                        let resolved = self.concrete(t);
                        if resolved.is_changed() {
                            changed = true;
                        }
                        resolved.into_owned()
                    })
                    .collect();

                if changed {
                    Resolved::New(Type::Union(Union { types }))
                } else {
                    Resolved::Same(typ)
                }
            }
            Type::Tuple(types, variadic) => {
                let mut changed = false;
                let resolved_types: Vec<Type> = types
                    .iter()
                    .map(|t| {
                        let resolved = self.concrete(t);
                        if resolved.is_changed() {
                            changed = true;
                        }
                        resolved.into_owned()
                    })
                    .collect();

                if changed {
                    Resolved::New(Type::Tuple(resolved_types, *variadic))
                } else {
                    Resolved::Same(typ)
                }
            }
            _ => Resolved::Same(typ),
        }
    }

    /// Get the underlying type, unwrapping Optional/Validated/Named.
    pub fn underlying(&self, typ: &Type) -> Type {
        match typ {
            Type::Optional(inner) => self.underlying(inner),
            Type::Validated(validated) => self.underlying(&validated.typ),
            Type::Annotated(ann) => self.underlying(&ann.typ),
            Type::Named(named) => {
                if let Some(resolved) = named.obj.resolved_type() {
                    self.underlying(&resolved)
                } else {
                    typ.clone()
                }
            }
            _ => typ.clone(),
        }
    }
}

/// Resolve function parameters to FunctionParam types.
fn resolve_parameters(ctx: &Ctx<'_>, params: &ast::Parameters) -> Vec<FunctionParam> {
    let mut result = Vec::new();

    // Positional-only parameters
    for param in &params.posonlyargs {
        let typ = param
            .parameter
            .annotation
            .as_ref()
            .map(|a| ctx.resolve_expr(a))
            .unwrap_or(Type::Any);

        result.push(FunctionParam {
            name: Some(param.parameter.name.to_string()),
            typ,
            optional: param.default.is_some(),
            variadic: false,
            keyword_variadic: false,
            kind: ParamKind::PositionalOnly,
        });
    }

    // Regular parameters
    for param in &params.args {
        let typ = param
            .parameter
            .annotation
            .as_ref()
            .map(|a| ctx.resolve_expr(a))
            .unwrap_or(Type::Any);

        result.push(FunctionParam {
            name: Some(param.parameter.name.to_string()),
            typ,
            optional: param.default.is_some(),
            variadic: false,
            keyword_variadic: false,
            kind: ParamKind::Regular,
        });
    }

    // *args parameter
    if let Some(ref vararg) = params.vararg {
        let typ = vararg
            .annotation
            .as_ref()
            .map(|a| ctx.resolve_expr(a))
            .unwrap_or(Type::Any);

        result.push(FunctionParam {
            name: Some(vararg.name.to_string()),
            typ,
            optional: false,
            variadic: true,
            keyword_variadic: false,
            kind: ParamKind::Regular,
        });
    }

    // Keyword-only parameters
    for param in &params.kwonlyargs {
        let typ = param
            .parameter
            .annotation
            .as_ref()
            .map(|a| ctx.resolve_expr(a))
            .unwrap_or(Type::Any);

        result.push(FunctionParam {
            name: Some(param.parameter.name.to_string()),
            typ,
            optional: param.default.is_some(),
            variadic: false,
            keyword_variadic: false,
            kind: ParamKind::KeywordOnly,
        });
    }

    // **kwargs parameter
    if let Some(ref kwarg) = params.kwarg {
        let typ = kwarg
            .annotation
            .as_ref()
            .map(|a| ctx.resolve_expr(a))
            .unwrap_or(Type::Any);

        result.push(FunctionParam {
            name: Some(kwarg.name.to_string()),
            typ,
            optional: false,
            variadic: false,
            keyword_variadic: true,
            kind: ParamKind::Regular,
        });
    }

    result
}
