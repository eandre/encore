use super::object::Object;
use std::fmt::Debug;

/// The main type representation enum.
///
/// Each variant represents a different kind of type that can appear in Python
/// type annotations. This is modeled after Python's `typing` module semantics.
#[derive(Debug, Clone)]
pub enum Type<'py> {
    /// Basic/primitive types: `str`, `int`, `float`, `bool`, `bytes`, `None`, etc.
    Basic(Basic),

    /// List of another type, e.g., `List[int]`
    List(Box<Type<'py>>),

    /// Dictionary type, e.g., `Dict[str, int]`
    Dict(Box<Type<'py>>, Box<Type<'py>>),

    /// Union type, e.g., `Union[int, str]`
    Union(Vec<Type<'py>>),

    /// Tuple type, e.g., `Tuple[int, str]`
    Tuple(Vec<Type<'py>>),

    /// A named type referencing a class or other named object.
    Named(Named<'py>),

    /// A literal type.
    Literal(Literal),

    /// A type parameter (generic type), e.g., `T` in `List[T]`.
    /// The usize represents the index of the type parameter in the declaration.
    TypeParamRefType(usize),

    /// A TypedDict type, representing a dictionary with specific typed fields.
    TypedDict(TypedDict<'py>),

    /// A class type, representing a class with typed fields.
    ClassType(ClassType<'py>),
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
    /// `None` type
    None,
    /// `Any` type
    Any,
    /// `never` type
    Never,
}

/// A named type, e.g., a class or custom type.
#[derive(Clone)]
pub struct Named<'py> {
    /// The object that this named type refers to.
    pub obj: &'py dyn Object,

    /// The type arguments used to instantiate this named type.
    pub type_args: Vec<Type<'py>>,
}

impl Debug for Named<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Named")
            .field("name", &self.obj.name())
            .field("type_args", &self.type_args)
            .finish()
    }
}

#[derive(Debug, Clone)]
pub enum Literal {
    String(String),
    Boolean(bool),
    Int(i64),
    Float(f64),
    BigInt(String),
}

/// TypedDict represents a TypedDict type with named, typed fields.
///
/// Example:
/// ```python
/// class Person(TypedDict):
///     name: str
///     age: int
/// ```
#[derive(Debug, Clone)]
pub struct TypedDict<'py> {
    /// The name of the TypedDict class.
    pub name: String,

    /// The fields of the TypedDict.
    pub fields: Vec<Field<'py>>,

    /// Whether this TypedDict has `total=False`, making all fields optional.
    pub total: bool,
}

/// ClassType represents a regular class with typed fields.
///
/// Example:
/// ```python
/// class Person:
///     name: str
///     age: int
/// ```
#[derive(Debug, Clone)]
pub struct ClassType<'py> {
    /// The name of the struct/class.
    pub name: String,

    /// The fields of the struct.
    pub fields: Vec<Field<'py>>,
}

/// Field represents a named field with a type, used in TypedDict and ClassType.
#[derive(Debug, Clone)]
pub struct Field<'py> {
    /// The name of the field.
    pub name: String,

    /// The type of the field.
    pub typ: Type<'py>,

    /// Whether the field is optional (has a default value or is marked Optional).
    pub optional: bool,

    /// Documentation for the field (from comments/docstrings).
    pub doc: Option<String>,
}

/// FuncType represents a function's type signature.
#[derive(Debug, Clone)]
pub struct FuncType<'py> {
    /// The function's parameters.
    pub params: Vec<Param<'py>>,

    /// The function's return type. None means no return type annotation.
    pub returns: Option<Box<Type<'py>>>,
}

/// Param represents a function parameter.
#[derive(Debug, Clone)]
pub struct Param<'py> {
    /// The parameter's name.
    pub name: String,

    /// The parameter's type. None means no type annotation.
    pub typ: Option<Type<'py>>,

    /// Whether this parameter has a default value.
    pub has_default: bool,

    /// The kind of parameter (positional, keyword, etc.).
    pub kind: ParamKind,
}

/// ParamKind represents the kind of a function parameter.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ParamKind {
    /// A regular positional or keyword parameter.
    Regular,
    /// A variadic positional parameter (*args).
    VarPositional,
    /// A variadic keyword parameter (**kwargs).
    VarKeyword,
    /// A positional-only parameter (before / in function signature).
    PositionalOnly,
    /// A keyword-only parameter (after * in function signature).
    KeywordOnly,
}
