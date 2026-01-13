//! Visitor pattern for type tree traversal.
//!
//! This module provides the `Visit` and `VisitWith` traits for implementing
//! custom operations on type trees using the visitor pattern.
//!
//! # Example
//!
//! ```ignore
//! struct TypeCollector {
//!     basic_types: Vec<Basic>,
//! }
//!
//! impl Visit for TypeCollector {
//!     fn visit_basic(&mut self, basic: &Basic) {
//!         self.basic_types.push(*basic);
//!     }
//! }
//!
//! let mut collector = TypeCollector { basic_types: Vec::new() };
//! some_type.visit_with(&mut collector);
//! ```

use std::collections::HashSet;

use super::object::{ObjectId, ResolveState};
use super::typ::*;
use super::validation;

/// Trait for visiting type trees.
///
/// Implement this trait to perform custom operations while traversing types.
/// Default implementations are provided for all methods that simply recurse
/// into children.
pub trait Visit {
    /// Get the resolve state for looking up named types.
    fn resolve_state(&self) -> Option<&ResolveState> {
        None
    }

    /// Track seen declarations to prevent infinite recursion.
    fn seen_decls(&mut self) -> &mut HashSet<ObjectId>;

    /// Visit a Type node.
    fn visit_type(&mut self, typ: &Type) {
        match typ {
            Type::Basic(basic) => self.visit_basic(basic),
            Type::List(inner) => self.visit_list(inner),
            Type::Set(inner) => self.visit_set(inner),
            Type::FrozenSet(inner) => self.visit_frozenset(inner),
            Type::Dict(key, value) => self.visit_dict(key, value),
            Type::Tuple(types, variadic) => self.visit_tuple(types, *variadic),
            Type::Union(union) => self.visit_union(union),
            Type::Optional(inner) => self.visit_optional(inner),
            Type::Interface(iface) => self.visit_interface(iface),
            Type::Class(class) => self.visit_class(class),
            Type::Enum(enum_type) => self.visit_enum(enum_type),
            Type::Named(named) => self.visit_named(named),
            Type::Literal(lit) => self.visit_literal(lit),
            Type::Generic(generic) => self.visit_generic(generic),
            Type::Validated(validated) => self.visit_validated(validated),
            Type::Validation(expr) => self.visit_validation(expr),
            Type::Custom(custom) => self.visit_custom(custom),
            Type::Callable(func) => self.visit_callable(func),
            Type::This(this) => self.visit_this(this),
            Type::Any => self.visit_any(),
            Type::Never => self.visit_never(),
            Type::TypeVar(tv) => self.visit_typevar(tv),
            Type::Annotated(ann) => self.visit_annotated(ann),
            Type::TypeAliasRef(alias) => self.visit_type_alias_ref(alias),
        }
    }

    /// Visit a Basic type.
    fn visit_basic(&mut self, _basic: &Basic) {}

    /// Visit a List type.
    fn visit_list(&mut self, inner: &Type) {
        self.visit_type(inner);
    }

    /// Visit a Set type.
    fn visit_set(&mut self, inner: &Type) {
        self.visit_type(inner);
    }

    /// Visit a FrozenSet type.
    fn visit_frozenset(&mut self, inner: &Type) {
        self.visit_type(inner);
    }

    /// Visit a Dict type.
    fn visit_dict(&mut self, key: &Type, value: &Type) {
        self.visit_type(key);
        self.visit_type(value);
    }

    /// Visit a Tuple type.
    fn visit_tuple(&mut self, types: &[Type], _variadic: bool) {
        for typ in types {
            self.visit_type(typ);
        }
    }

    /// Visit a Union type.
    fn visit_union(&mut self, union: &Union) {
        for typ in &union.types {
            self.visit_type(typ);
        }
    }

    /// Visit an Optional type.
    fn visit_optional(&mut self, inner: &Type) {
        self.visit_type(inner);
    }

    /// Visit an Interface type.
    fn visit_interface(&mut self, iface: &Interface) {
        for field in &iface.fields {
            self.visit_interface_field(field);
        }
        if let Some(ref index) = iface.index_signature {
            self.visit_type(&index.key);
            self.visit_type(&index.value);
        }
    }

    /// Visit an interface field.
    fn visit_interface_field(&mut self, field: &InterfaceField) {
        self.visit_type(&field.typ);
    }

    /// Visit a Class type.
    fn visit_class(&mut self, class: &ClassType) {
        // Prevent infinite recursion for recursive types
        if self.seen_decls().insert(class.obj.id) {
            for arg in &class.type_arguments {
                self.visit_type(arg);
            }
            for attr in &class.class_attrs {
                self.visit_interface_field(attr);
            }
            for attr in &class.instance_attrs {
                self.visit_interface_field(attr);
            }
            for method in &class.methods {
                self.visit_callable(&method.signature);
            }
            for base in &class.bases {
                self.visit_type(base);
            }
        }
    }

    /// Visit an Enum type.
    fn visit_enum(&mut self, enum_type: &EnumType) {
        if self.seen_decls().insert(enum_type.obj.id) {
            for member in &enum_type.members {
                self.visit_enum_member(member);
            }
        }
    }

    /// Visit an enum member.
    fn visit_enum_member(&mut self, _member: &EnumMember) {}

    /// Visit a Named type reference.
    fn visit_named(&mut self, named: &Named) {
        // Prevent infinite recursion
        if self.seen_decls().insert(named.obj.id) {
            for arg in &named.type_arguments {
                self.visit_type(arg);
            }
            // Optionally resolve and visit the underlying type
            if let Some(typ) = named.obj.resolved_type() {
                self.visit_type(&typ);
            }
        }
    }

    /// Visit a Literal type.
    fn visit_literal(&mut self, _lit: &LiteralType) {}

    /// Visit a Generic type construct.
    fn visit_generic(&mut self, generic: &Generic) {
        match generic {
            Generic::TypeParam(param) => self.visit_type_param(param),
            Generic::Indexed(indexed) => self.visit_indexed(indexed),
            Generic::Mapped(mapped) => self.visit_mapped(mapped),
            Generic::MappedKeyType(key) => self.visit_mapped_key_type(key),
            Generic::Keyof(keyof) => self.visit_keyof(keyof),
            Generic::Intersection(inter) => self.visit_intersection(inter),
        }
    }

    /// Visit a type parameter.
    fn visit_type_param(&mut self, param: &TypeParam) {
        if let Some(ref constraint) = param.constraint {
            self.visit_type(constraint);
        }
    }

    /// Visit an indexed type.
    fn visit_indexed(&mut self, indexed: &Indexed) {
        self.visit_type(&indexed.object_type);
        self.visit_type(&indexed.index_type);
    }

    /// Visit a mapped type.
    fn visit_mapped(&mut self, mapped: &Mapped) {
        self.visit_type(&mapped.in_type);
        self.visit_type(&mapped.value_type);
        if let Some(ref as_type) = mapped.as_type {
            self.visit_type(as_type);
        }
    }

    /// Visit a mapped key type.
    fn visit_mapped_key_type(&mut self, _key: &MappedKeyType) {}

    /// Visit a keyof type.
    fn visit_keyof(&mut self, keyof: &Keyof) {
        self.visit_type(&keyof.typ);
    }

    /// Visit an intersection type.
    fn visit_intersection(&mut self, inter: &Intersection) {
        for typ in &inter.types {
            self.visit_type(typ);
        }
    }

    /// Visit a Validated type.
    fn visit_validated(&mut self, validated: &Validated) {
        self.visit_type(&validated.typ);
        self.visit_validation(&validated.expr);
    }

    /// Visit a validation expression.
    fn visit_validation(&mut self, expr: &validation::Expr) {
        match expr {
            validation::Expr::Rule(rule) => self.visit_validation_rule(rule),
            validation::Expr::And(exprs) | validation::Expr::Or(exprs) => {
                for e in exprs {
                    self.visit_validation(e);
                }
            }
        }
    }

    /// Visit a validation rule.
    fn visit_validation_rule(&mut self, _rule: &validation::Rule) {}

    /// Visit a Custom type.
    fn visit_custom(&mut self, _custom: &Custom) {}

    /// Visit a Callable/function type.
    fn visit_callable(&mut self, func: &FunctionType) {
        for param in &func.params {
            self.visit_function_param(param);
        }
        self.visit_type(&func.return_type);
        if let Some(ref type_params) = func.type_params {
            for param in type_params {
                self.visit_type_param(param);
            }
        }
    }

    /// Visit a function parameter.
    fn visit_function_param(&mut self, param: &FunctionParam) {
        self.visit_type(&param.typ);
    }

    /// Visit a This type.
    fn visit_this(&mut self, _this: &This) {}

    /// Visit the Any type.
    fn visit_any(&mut self) {}

    /// Visit the Never type.
    fn visit_never(&mut self) {}

    /// Visit a TypeVar.
    fn visit_typevar(&mut self, tv: &TypeVar) {
        for constraint in &tv.constraints {
            self.visit_type(constraint);
        }
        if let Some(ref bound) = tv.bound {
            self.visit_type(bound);
        }
    }

    /// Visit an Annotated type.
    fn visit_annotated(&mut self, ann: &Annotated) {
        self.visit_type(&ann.typ);
        for meta in &ann.metadata {
            self.visit_annotation_metadata(meta);
        }
    }

    /// Visit annotation metadata.
    fn visit_annotation_metadata(&mut self, meta: &AnnotationMetadata) {
        if let AnnotationMetadata::Validation(expr) = meta {
            self.visit_validation(expr);
        }
    }

    /// Visit a type alias reference.
    fn visit_type_alias_ref(&mut self, alias: &TypeAliasRef) {
        if self.seen_decls().insert(alias.obj.id) {
            for arg in &alias.type_arguments {
                self.visit_type(arg);
            }
        }
    }
}

/// Trait for types that can be visited.
pub trait VisitWith<V: Visit> {
    /// Accept a visitor.
    fn visit_with(&self, visitor: &mut V);

    /// Visit children of this node.
    fn visit_children_with(&self, visitor: &mut V);
}

impl<V: Visit> VisitWith<V> for Type {
    fn visit_with(&self, visitor: &mut V) {
        visitor.visit_type(self);
    }

    fn visit_children_with(&self, visitor: &mut V) {
        // This dispatches to the appropriate method which handles children
        visitor.visit_type(self);
    }
}

impl<V: Visit> VisitWith<V> for Interface {
    fn visit_with(&self, visitor: &mut V) {
        visitor.visit_interface(self);
    }

    fn visit_children_with(&self, visitor: &mut V) {
        for field in &self.fields {
            field.visit_with(visitor);
        }
    }
}

impl<V: Visit> VisitWith<V> for InterfaceField {
    fn visit_with(&self, visitor: &mut V) {
        visitor.visit_interface_field(self);
    }

    fn visit_children_with(&self, visitor: &mut V) {
        self.typ.visit_with(visitor);
    }
}

impl<V: Visit> VisitWith<V> for Union {
    fn visit_with(&self, visitor: &mut V) {
        visitor.visit_union(self);
    }

    fn visit_children_with(&self, visitor: &mut V) {
        for typ in &self.types {
            typ.visit_with(visitor);
        }
    }
}

impl<V: Visit> VisitWith<V> for FunctionType {
    fn visit_with(&self, visitor: &mut V) {
        visitor.visit_callable(self);
    }

    fn visit_children_with(&self, visitor: &mut V) {
        for param in &self.params {
            param.typ.visit_with(visitor);
        }
        self.return_type.visit_with(visitor);
    }
}

/// A simple visitor that collects all Basic types in a type tree.
pub struct BasicTypeCollector {
    /// Collected basic types.
    pub basic_types: Vec<Basic>,
    /// Seen declarations for cycle prevention.
    seen: HashSet<ObjectId>,
}

impl BasicTypeCollector {
    /// Create a new collector.
    pub fn new() -> Self {
        BasicTypeCollector {
            basic_types: Vec::new(),
            seen: HashSet::new(),
        }
    }

    /// Collect all basic types from a type.
    pub fn collect(typ: &Type) -> Vec<Basic> {
        let mut collector = Self::new();
        typ.visit_with(&mut collector);
        collector.basic_types
    }
}

impl Default for BasicTypeCollector {
    fn default() -> Self {
        Self::new()
    }
}

impl Visit for BasicTypeCollector {
    fn seen_decls(&mut self) -> &mut HashSet<ObjectId> {
        &mut self.seen
    }

    fn visit_basic(&mut self, basic: &Basic) {
        self.basic_types.push(*basic);
    }
}

/// A visitor that checks if a type contains any generic type parameters.
pub struct ContainsGenericChecker {
    /// Whether a generic type parameter was found.
    pub found: bool,
    /// Seen declarations for cycle prevention.
    seen: HashSet<ObjectId>,
}

impl ContainsGenericChecker {
    /// Create a new checker.
    pub fn new() -> Self {
        ContainsGenericChecker {
            found: false,
            seen: HashSet::new(),
        }
    }

    /// Check if a type contains generic parameters.
    pub fn check(typ: &Type) -> bool {
        let mut checker = Self::new();
        typ.visit_with(&mut checker);
        checker.found
    }
}

impl Default for ContainsGenericChecker {
    fn default() -> Self {
        Self::new()
    }
}

impl Visit for ContainsGenericChecker {
    fn seen_decls(&mut self) -> &mut HashSet<ObjectId> {
        &mut self.seen
    }

    fn visit_generic(&mut self, _generic: &Generic) {
        self.found = true;
    }

    fn visit_typevar(&mut self, _tv: &TypeVar) {
        self.found = true;
    }
}

/// A visitor that collects all named type references.
pub struct NamedTypeCollector {
    /// Collected named types (by ObjectId).
    pub named_types: HashSet<ObjectId>,
    /// Seen declarations for cycle prevention.
    seen: HashSet<ObjectId>,
}

impl NamedTypeCollector {
    /// Create a new collector.
    pub fn new() -> Self {
        NamedTypeCollector {
            named_types: HashSet::new(),
            seen: HashSet::new(),
        }
    }

    /// Collect all named type references from a type.
    pub fn collect(typ: &Type) -> HashSet<ObjectId> {
        let mut collector = Self::new();
        typ.visit_with(&mut collector);
        collector.named_types
    }
}

impl Default for NamedTypeCollector {
    fn default() -> Self {
        Self::new()
    }
}

impl Visit for NamedTypeCollector {
    fn seen_decls(&mut self) -> &mut HashSet<ObjectId> {
        &mut self.seen
    }

    fn visit_named(&mut self, named: &Named) {
        self.named_types.insert(named.obj.id);
        // Still visit children
        for arg in &named.type_arguments {
            self.visit_type(arg);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_basic_type_collector() {
        let typ = Type::Union(Union {
            types: vec![Type::Basic(Basic::Str), Type::Basic(Basic::Int)],
        });

        let basics = BasicTypeCollector::collect(&typ);
        assert_eq!(basics.len(), 2);
        assert!(basics.contains(&Basic::Str));
        assert!(basics.contains(&Basic::Int));
    }

    #[test]
    fn test_contains_generic_checker() {
        let simple = Type::Basic(Basic::Str);
        assert!(!ContainsGenericChecker::check(&simple));

        let generic = Type::Generic(Generic::TypeParam(TypeParam {
            name: "T".to_string(),
            idx: 0,
            constraint: None,
            variance: Variance::Invariant,
        }));
        assert!(ContainsGenericChecker::check(&generic));
    }
}
