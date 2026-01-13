//! Python type representation and resolution system.
//!
//! This module provides a comprehensive type system for parsing Python type annotations,
//! similar to how pydantic handles type introspection but designed for static analysis.
//!
//! # Architecture
//!
//! The type system follows these key design patterns:
//!
//! - **Enum-based polymorphism**: All type variants are represented as enum variants
//!   rather than trait objects, enabling exhaustive pattern matching.
//!
//! - **Reference counting**: Named types reference their definitions via `Rc<Object>`,
//!   allowing shared ownership without cloning.
//!
//! - **Interior mutability**: `RefCell` is used for lazy type resolution with caching.
//!
//! - **Structural comparison**: Types are compared structurally, not by identity.
//!
//! # Module Organization
//!
//! - [`typ`]: Core type representations (Type enum and all variants)
//! - [`object`]: Named type objects and resolution state
//! - [`type_resolve`]: Type resolution from AST expressions
//! - [`resolved`]: Lazy resolution tracking
//! - [`validation`]: Validation expressions (like pydantic validators)
//! - [`binding`]: Pattern binding extraction for destructuring
//! - [`visitor`]: Visitor pattern for type tree traversal

mod binding;
mod object;
mod resolved;
mod typ;
mod type_resolve;
mod validation;
mod visitor;

#[cfg(test)]
mod tests;

// Re-export core types
pub use typ::{
    Basic, ClassType, Custom, EnumMember, EnumType, FieldName, FunctionParam, FunctionType,
    Generic, Indexed, Interface, InterfaceField, Intersection, Keyof, Literal, LiteralType,
    Mapped, Named, This, Type, TypeParam, Union, Validated,
};

// Re-export object types
pub use object::{
    CheckState, Class, Enum, Func, Module, ModuleId, Namespace, Object, ObjectId, ObjectKind,
    ResolveState, TypeAlias, Var,
};

// Re-export type resolution
pub use type_resolve::{Ctx, TypeChecker};

// Re-export resolution tracking
pub use resolved::Resolved;

// Re-export validation types
pub use validation::{Expr as ValidationExpr, Is, N as ValidationN, Rule};

// Re-export binding types
pub use binding::{BindingPat, DestructuringExpr, DestructuringKey};

// Re-export visitor traits
pub use visitor::{Visit, VisitWith};
