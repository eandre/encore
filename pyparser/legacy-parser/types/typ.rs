//! Core type representations for Python type annotations.
//!
//! This module defines the `Type` enum and all related structures that represent
//! Python's type system as understood through type hints and annotations.

use std::rc::Rc;

use super::object::Object;
use super::validation;

/// The main type representation enum.
///
/// Each variant represents a different kind of type that can appear in Python
/// type annotations. This is modeled after Python's `typing` module semantics.
#[derive(Debug, Clone)]
pub enum Type {
    /// Basic/primitive types: `str`, `int`, `float`, `bool`, `bytes`, `None`, etc.
    Basic(Basic),

    /// List type: `list[T]` or `List[T]`
    List(Box<Type>),

    /// Set type: `set[T]` or `Set[T]`
    Set(Box<Type>),

    /// FrozenSet type: `frozenset[T]` or `FrozenSet[T]`
    FrozenSet(Box<Type>),

    /// Dictionary type: `dict[K, V]` or `Dict[K, V]`
    Dict(Box<Type>, Box<Type>),

    /// Tuple type: `tuple[T, U, V]` or `Tuple[T, U, V]`
    /// The bool indicates if this is a variable-length tuple (tuple[T, ...])
    Tuple(Vec<Type>, bool),

    /// Union type: `Union[A, B, C]` or `A | B | C`
    Union(Union),

    /// Optional type: `Optional[T]` (syntactic sugar for `Union[T, None]`)
    Optional(Box<Type>),

    /// Interface/structured type (similar to TypedDict or Protocol)
    Interface(Interface),

    /// Class type with methods and attributes
    Class(ClassType),

    /// Enum type
    Enum(EnumType),

    /// Named type reference: `MyClass`, `MyTypeAlias[A, B]`
    Named(Named),

    /// Literal type: `Literal["foo", "bar"]`, `Literal[1, 2, 3]`
    Literal(LiteralType),

    /// Generic type constructs (type parameters, mapped types, etc.)
    Generic(Generic),

    /// Type with validation rules attached (like pydantic Field validators)
    Validated(Validated),

    /// Standalone validation expression
    Validation(validation::Expr),

    /// Custom/special types (e.g., Decimal, datetime)
    Custom(Custom),

    /// Callable type: `Callable[[A, B], R]`
    Callable(FunctionType),

    /// The `self` type in method signatures
    This(This),

    /// Any type: `Any`
    Any,

    /// Never/NoReturn type: `NoReturn`, `Never`
    Never,

    /// Type variable: `TypeVar('T')`
    TypeVar(TypeVar),

    /// Annotated type: `Annotated[T, metadata1, metadata2]`
    Annotated(Annotated),

    /// Type alias: `TypeAlias` or `type X = ...` (PEP 695)
    TypeAliasRef(TypeAliasRef),
}

impl Type {
    /// Check if two types are structurally identical.
    pub fn identical(&self, other: &Type) -> bool {
        match (self, other) {
            (Type::Basic(a), Type::Basic(b)) => a == b,
            (Type::List(a), Type::List(b)) => a.identical(b),
            (Type::Set(a), Type::Set(b)) => a.identical(b),
            (Type::FrozenSet(a), Type::FrozenSet(b)) => a.identical(b),
            (Type::Dict(ak, av), Type::Dict(bk, bv)) => ak.identical(bk) && av.identical(bv),
            (Type::Tuple(a, a_var), Type::Tuple(b, b_var)) => {
                a_var == b_var
                    && a.len() == b.len()
                    && a.iter().zip(b.iter()).all(|(x, y)| x.identical(y))
            }
            (Type::Union(a), Type::Union(b)) => {
                a.types.len() == b.types.len()
                    && a.types
                        .iter()
                        .zip(b.types.iter())
                        .all(|(x, y)| x.identical(y))
            }
            (Type::Optional(a), Type::Optional(b)) => a.identical(b),
            (Type::Named(a), Type::Named(b)) => {
                a.obj.id == b.obj.id
                    && a.type_arguments.len() == b.type_arguments.len()
                    && a.type_arguments
                        .iter()
                        .zip(b.type_arguments.iter())
                        .all(|(x, y)| x.identical(y))
            }
            (Type::Literal(a), Type::Literal(b)) => a.identical(b),
            (Type::Any, Type::Any) => true,
            (Type::Never, Type::Never) => true,
            (Type::This(_), Type::This(_)) => true,
            _ => false,
        }
    }

    /// Returns true if this type is Optional or contains None in a Union.
    pub fn is_optional(&self) -> bool {
        match self {
            Type::Optional(_) => true,
            Type::Basic(Basic::None) => true,
            Type::Union(u) => u
                .types
                .iter()
                .any(|t| matches!(t, Type::Basic(Basic::None))),
            _ => false,
        }
    }

    /// Attempt to merge this type with another into a union, simplifying where possible.
    pub fn union_merge(self, other: Type) -> Type {
        if self.identical(&other) {
            return self;
        }
        match (self, other) {
            (Type::Union(mut a), Type::Union(b)) => {
                for t in b.types {
                    if !a.types.iter().any(|existing| existing.identical(&t)) {
                        a.types.push(t);
                    }
                }
                Type::Union(a)
            }
            (Type::Union(mut u), t) | (t, Type::Union(mut u)) => {
                if !u.types.iter().any(|existing| existing.identical(&t)) {
                    u.types.push(t);
                }
                Type::Union(u)
            }
            (a, b) => Type::Union(Union { types: vec![a, b] }),
        }
    }

    /// Unwrap the type if it's Optional, returning the inner type.
    pub fn unwrap_optional(&self) -> Option<&Type> {
        match self {
            Type::Optional(inner) => Some(inner),
            _ => None,
        }
    }
}

/// Basic/primitive Python types.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Basic {
    /// `str` type
    Str,
    /// `int` type
    Int,
    /// `float` type
    Float,
    /// `bool` type
    Bool,
    /// `bytes` type
    Bytes,
    /// `bytearray` type
    ByteArray,
    /// `None` type (NoneType)
    None,
    /// `object` type (base of all classes)
    Object,
    /// `complex` type
    Complex,
    /// `type` type (metatype)
    Type,
    /// `ellipsis` type (...)
    Ellipsis,
}

impl Basic {
    /// Get the Python type name for this basic type.
    pub fn type_name(&self) -> &'static str {
        match self {
            Basic::Str => "str",
            Basic::Int => "int",
            Basic::Float => "float",
            Basic::Bool => "bool",
            Basic::Bytes => "bytes",
            Basic::ByteArray => "bytearray",
            Basic::None => "None",
            Basic::Object => "object",
            Basic::Complex => "complex",
            Basic::Type => "type",
            Basic::Ellipsis => "...",
        }
    }
}

/// Union type: `Union[A, B, C]` or `A | B | C`.
#[derive(Debug, Clone)]
pub struct Union {
    /// The types in the union.
    pub types: Vec<Type>,
}

impl Union {
    /// Create a new union from a list of types, flattening nested unions.
    pub fn new(types: Vec<Type>) -> Self {
        let mut flattened = Vec::new();
        for t in types {
            match t {
                Type::Union(u) => flattened.extend(u.types),
                other => flattened.push(other),
            }
        }
        Union { types: flattened }
    }

    /// Simplify the union by removing duplicates.
    pub fn simplify(mut self) -> Type {
        // Remove duplicates
        let mut deduped = Vec::new();
        for t in self.types {
            if !deduped.iter().any(|existing: &Type| existing.identical(&t)) {
                deduped.push(t);
            }
        }
        self.types = deduped;

        // If only one type remains, unwrap
        if self.types.len() == 1 {
            self.types.pop().unwrap()
        } else {
            Type::Union(self)
        }
    }
}

/// Interface type representing structured types like TypedDict or Protocol.
#[derive(Debug, Clone)]
pub struct Interface {
    /// The fields of the interface.
    pub fields: Vec<InterfaceField>,
    /// Optional index signature: `{[key: K]: V}`
    pub index_signature: Option<IndexSignature>,
    /// Whether this interface represents a total TypedDict (all keys required).
    pub total: bool,
}

/// A field in an interface type.
#[derive(Debug, Clone)]
pub struct InterfaceField {
    /// Source location range.
    pub range: ruff_text_size::TextRange,
    /// Field name.
    pub name: FieldName,
    /// Whether the field is optional (has a default or is marked Optional).
    pub optional: bool,
    /// Whether the field is read-only.
    pub readonly: bool,
    /// The field's type.
    pub typ: Type,
    /// Documentation comment if any.
    pub doc: Option<String>,
}

/// Field name representation.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum FieldName {
    /// String key name.
    String(String),
    /// Computed key (symbol or expression).
    Computed(String),
}

impl FieldName {
    /// Get the string representation of the field name.
    pub fn as_str(&self) -> &str {
        match self {
            FieldName::String(s) => s,
            FieldName::Computed(s) => s,
        }
    }
}

/// Index signature for interfaces: `{[key: K]: V}`.
#[derive(Debug, Clone)]
pub struct IndexSignature {
    /// Key type.
    pub key: Box<Type>,
    /// Value type.
    pub value: Box<Type>,
}

/// Class type with methods and attributes.
#[derive(Debug, Clone)]
pub struct ClassType {
    /// Reference to the class object definition.
    pub obj: Rc<Object>,
    /// Type arguments if the class is generic.
    pub type_arguments: Vec<Type>,
    /// Class attributes (class variables).
    pub class_attrs: Vec<InterfaceField>,
    /// Instance attributes.
    pub instance_attrs: Vec<InterfaceField>,
    /// Methods.
    pub methods: Vec<MethodType>,
    /// Base classes.
    pub bases: Vec<Type>,
}

/// Method type in a class.
#[derive(Debug, Clone)]
pub struct MethodType {
    /// Method name.
    pub name: String,
    /// Method signature.
    pub signature: FunctionType,
    /// Whether this is a classmethod.
    pub is_classmethod: bool,
    /// Whether this is a staticmethod.
    pub is_staticmethod: bool,
    /// Whether this is a property.
    pub is_property: bool,
}

/// Enum type.
#[derive(Debug, Clone)]
pub struct EnumType {
    /// Reference to the enum object definition.
    pub obj: Rc<Object>,
    /// Enum members.
    pub members: Vec<EnumMember>,
}

/// An enum member.
#[derive(Debug, Clone)]
pub struct EnumMember {
    /// Member name.
    pub name: String,
    /// Member value (if known).
    pub value: Option<Literal>,
}

/// Named type reference: `MyClass`, `MyTypeAlias[T]`.
#[derive(Debug, Clone)]
pub struct Named {
    /// Reference to the type object definition.
    pub obj: Rc<Object>,
    /// Type arguments for generic types.
    pub type_arguments: Vec<Type>,
}

/// Literal type values.
#[derive(Debug, Clone)]
pub enum Literal {
    /// String literal: `"foo"`
    String(String),
    /// Bytes literal: `b"foo"`
    Bytes(Vec<u8>),
    /// Integer literal: `42`
    Int(i64),
    /// Float literal: `3.14`
    Float(f64),
    /// Boolean literal: `True` or `False`
    Bool(bool),
    /// None literal
    None,
}

impl Literal {
    /// Check if two literals are identical.
    pub fn identical(&self, other: &Literal) -> bool {
        match (self, other) {
            (Literal::String(a), Literal::String(b)) => a == b,
            (Literal::Bytes(a), Literal::Bytes(b)) => a == b,
            (Literal::Int(a), Literal::Int(b)) => a == b,
            (Literal::Float(a), Literal::Float(b)) => (a - b).abs() < f64::EPSILON,
            (Literal::Bool(a), Literal::Bool(b)) => a == b,
            (Literal::None, Literal::None) => true,
            _ => false,
        }
    }
}

/// Literal type: `Literal["foo", "bar"]`.
#[derive(Debug, Clone)]
pub struct LiteralType {
    /// The allowed literal values.
    pub values: Vec<Literal>,
}

impl LiteralType {
    /// Check if two literal types are identical.
    pub fn identical(&self, other: &LiteralType) -> bool {
        self.values.len() == other.values.len()
            && self
                .values
                .iter()
                .zip(other.values.iter())
                .all(|(a, b)| a.identical(b))
    }
}

/// Generic type constructs.
#[derive(Debug, Clone)]
pub enum Generic {
    /// Type parameter: `T` in a generic context.
    TypeParam(TypeParam),
    /// Indexed type: `T[K]`.
    Indexed(Indexed),
    /// Mapped type (similar to TypedDict comprehension).
    Mapped(Mapped),
    /// Key type within a mapped type context.
    MappedKeyType(MappedKeyType),
    /// Keyof type: all string keys of a type.
    Keyof(Keyof),
    /// Intersection of types (Protocol combination).
    Intersection(Intersection),
}

/// Type parameter reference.
#[derive(Debug, Clone)]
pub struct TypeParam {
    /// Name of the type parameter.
    pub name: String,
    /// Index in the type parameter list.
    pub idx: usize,
    /// Constraint: `T: SomeProtocol` or `T(bound=SomeClass)`.
    pub constraint: Option<Box<Type>>,
    /// Variance: covariant, contravariant, or invariant.
    pub variance: Variance,
}

/// Type parameter variance.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Variance {
    /// Invariant (default).
    Invariant,
    /// Covariant (TypeVar with covariant=True).
    Covariant,
    /// Contravariant (TypeVar with contravariant=True).
    Contravariant,
}

/// TypeVar definition.
#[derive(Debug, Clone)]
pub struct TypeVar {
    /// TypeVar name.
    pub name: String,
    /// Constraints: specific types that are allowed.
    pub constraints: Vec<Type>,
    /// Bound: upper bound for the type variable.
    pub bound: Option<Box<Type>>,
    /// Variance.
    pub variance: Variance,
}

/// Indexed type access: `T[K]`.
#[derive(Debug, Clone)]
pub struct Indexed {
    /// The type being indexed.
    pub object_type: Box<Type>,
    /// The index type.
    pub index_type: Box<Type>,
}

/// Mapped type (conceptually similar to a comprehension over type keys).
#[derive(Debug, Clone)]
pub struct Mapped {
    /// The type to iterate over.
    pub in_type: Box<Type>,
    /// The value type expression.
    pub value_type: Box<Type>,
    /// Whether fields are optional in the result.
    pub optional: Option<bool>,
    /// Key remapping expression.
    pub as_type: Option<Box<Type>>,
}

/// Key type within a mapped type context.
#[derive(Debug, Clone)]
pub struct MappedKeyType {
    /// Name of the key variable.
    pub name: String,
}

/// Keyof type: string literal union of all keys.
#[derive(Debug, Clone)]
pub struct Keyof {
    /// The type to get keys from.
    pub typ: Box<Type>,
}

/// Intersection type (Protocol combination).
#[derive(Debug, Clone)]
pub struct Intersection {
    /// The types being intersected.
    pub types: Vec<Type>,
}

/// Type with validation rules.
#[derive(Debug, Clone)]
pub struct Validated {
    /// The underlying type.
    pub typ: Box<Type>,
    /// The validation expression.
    pub expr: validation::Expr,
}

/// Custom/special types.
#[derive(Debug, Clone)]
pub enum Custom {
    /// Decimal type for precise numeric values.
    Decimal,
    /// UUID type.
    UUID,
    /// Date type.
    Date,
    /// Time type.
    Time,
    /// DateTime type.
    DateTime,
    /// TimeDelta/Duration type.
    TimeDelta,
    /// Path type.
    Path,
    /// URL type.
    Url,
    /// Email type (validated string).
    Email,
    /// JSON type (arbitrary JSON-serializable data).
    Json,
    /// Regular expression pattern.
    Pattern,
}

/// Function/Callable type: `Callable[[A, B], R]`.
#[derive(Debug, Clone)]
pub struct FunctionType {
    /// Function parameters.
    pub params: Vec<FunctionParam>,
    /// Return type.
    pub return_type: Box<Type>,
    /// Type parameters if this is a generic function.
    pub type_params: Option<Vec<TypeParam>>,
    /// Whether this is an async function.
    pub is_async: bool,
}

/// Function parameter.
#[derive(Debug, Clone)]
pub struct FunctionParam {
    /// Parameter name (may be None for Callable types).
    pub name: Option<String>,
    /// Parameter type.
    pub typ: Type,
    /// Whether the parameter is optional (has a default).
    pub optional: bool,
    /// Whether this is a *args parameter.
    pub variadic: bool,
    /// Whether this is a **kwargs parameter.
    pub keyword_variadic: bool,
    /// Parameter kind.
    pub kind: ParamKind,
}

/// Function parameter kind.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ParamKind {
    /// Regular positional or keyword parameter.
    Regular,
    /// Positional-only parameter (before `/`).
    PositionalOnly,
    /// Keyword-only parameter (after `*`).
    KeywordOnly,
}

/// The `self` type in method signatures.
#[derive(Debug, Clone)]
pub struct This {
    /// Reference to the containing class.
    pub class_obj: Option<Rc<Object>>,
}

/// Annotated type: `Annotated[T, metadata1, metadata2]`.
#[derive(Debug, Clone)]
pub struct Annotated {
    /// The underlying type.
    pub typ: Box<Type>,
    /// Metadata annotations.
    pub metadata: Vec<AnnotationMetadata>,
}

/// Metadata in an Annotated type.
#[derive(Debug, Clone)]
pub enum AnnotationMetadata {
    /// Validation expression (e.g., from pydantic Field).
    Validation(validation::Expr),
    /// Documentation string.
    Doc(String),
    /// Field configuration.
    FieldConfig(FieldConfig),
    /// Custom/unknown metadata (preserved as-is).
    Custom(String),
}

/// Field configuration from pydantic Field or similar.
#[derive(Debug, Clone, Default)]
pub struct FieldConfig {
    /// Default value expression (as string).
    pub default: Option<String>,
    /// Default factory expression (as string).
    pub default_factory: Option<String>,
    /// Alias for serialization.
    pub alias: Option<String>,
    /// Field title.
    pub title: Option<String>,
    /// Field description.
    pub description: Option<String>,
    /// Example values.
    pub examples: Vec<String>,
    /// Whether to exclude from serialization.
    pub exclude: Option<bool>,
    /// Whether to include in serialization.
    pub include: Option<bool>,
    /// Whether the field is deprecated.
    pub deprecated: Option<bool>,
}

/// Type alias reference.
#[derive(Debug, Clone)]
pub struct TypeAliasRef {
    /// Reference to the type alias object.
    pub obj: Rc<Object>,
    /// Type arguments for generic aliases.
    pub type_arguments: Vec<Type>,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_basic_identical() {
        assert!(Type::Basic(Basic::Str).identical(&Type::Basic(Basic::Str)));
        assert!(!Type::Basic(Basic::Str).identical(&Type::Basic(Basic::Int)));
    }

    #[test]
    fn test_union_merge() {
        let a = Type::Basic(Basic::Str);
        let b = Type::Basic(Basic::Int);
        let union = a.union_merge(b);

        if let Type::Union(u) = union {
            assert_eq!(u.types.len(), 2);
        } else {
            panic!("Expected Union type");
        }
    }

    #[test]
    fn test_union_simplify() {
        let union = Union::new(vec![
            Type::Basic(Basic::Str),
            Type::Basic(Basic::Str), // duplicate
        ]);
        let simplified = union.simplify();

        // Should simplify to just Str
        assert!(matches!(simplified, Type::Basic(Basic::Str)));
    }

    #[test]
    fn test_literal_identical() {
        assert!(Literal::String("foo".to_string()).identical(&Literal::String("foo".to_string())));
        assert!(!Literal::String("foo".to_string()).identical(&Literal::String("bar".to_string())));
        assert!(!Literal::Int(42).identical(&Literal::String("42".to_string())));
    }
}
