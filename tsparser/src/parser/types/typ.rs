use crate::parser::types::{ResolveState, object};
use std::collections::HashMap;
use std::hash::{Hash, Hasher};
use std::rc::Rc;

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct TypeArgId(usize);

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum Type {
    /// strings, etc
    Basic(Basic),
    /// T[], Array<T>
    Array(Box<Type>),
    /// { foo: string }
    Interface(Interface),
    /// a | b | c
    Union(Vec<Type>),
    /// [string, number]
    Tuple(Vec<Type>),
    /// "foo"
    Literal(Literal),
    /// class Foo {}
    Class(ClassType),

    /// A named type, with optional type arguments.
    Named(Named),

    /// e.g. "string?" in tuples
    Optional(Box<Type>),

    /// "this", see https://www.typescriptlang.org/docs/handbook/advanced-types.html#polymorphic-this-types
    This,

    Generic(Generic),
}

impl Type {
    pub fn is_void(&self) -> bool {
        matches!(self, Type::Basic(Basic::Void))
    }
}

impl Type {
    pub fn identical(&self, other: &Type) -> bool {
        match (self, other) {
            (Type::Basic(a), Type::Basic(b)) => a == b,
            (Type::Array(a), Type::Array(b)) => a.identical(b),
            (Type::Interface(a), Type::Interface(b)) => a.identical(b),
            (Type::Union(a), Type::Union(b)) => a.iter().zip(b).all(|(a, b)| a.identical(b)),
            (Type::Tuple(a), Type::Tuple(b)) => a.iter().zip(b).all(|(a, b)| a.identical(b)),
            (Type::Literal(a), Type::Literal(b)) => a == b,
            (Type::Class(a), Type::Class(b)) => a.identical(b),
            (Type::Named(a), Type::Named(b)) => a.identical(b),
            (Type::Optional(a), Type::Optional(b)) => a.identical(b),
            (Type::This, Type::This) => true,
            (Type::Generic(a), Type::Generic(b)) => a.identical(b),
            _ => false,
        }
    }

    /// Returns a unified type that merges `self` and `other`, if possible.
    /// If the types cannot be merged, returns `Err((self, other))`.
    pub(super) fn unify(self, other: Type) -> Result<Type, (Type, Type)> {
        match (self, other) {
            // 'any' and any type unify to 'any'.
            (Type::Basic(Basic::Any), _) | (_, Type::Basic(Basic::Any)) => {
                Ok(Type::Basic(Basic::Any))
            }

            // Type literals unify with their basic type
            (Type::Basic(basic), Type::Literal(lit)) | (Type::Literal(lit), Type::Basic(basic))
                if basic == lit.basic() =>
            {
                Ok(Type::Basic(basic))
            }

            // TODO more rules?

            // Identical types unify.
            (this, other) if this.identical(&other) => Ok(this),

            // Otherwise no unification is possible.
            (this, other) => Err((this, other)),
        }
    }

    pub(super) fn unify_or_union(self, other: Type) -> Type {
        match self.unify(other) {
            Ok(typ) => typ,
            Err((this, other)) => Type::Union(vec![this, other]),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Basic {
    Any,
    String,
    Boolean,
    Number,
    Object,
    BigInt,
    Symbol,
    Undefined,
    Null,
    Void,
    Unknown,
    Never,
}

#[derive(Debug, Clone)]
pub enum Literal {
    String(String),
    Boolean(bool),
    Number(f64),
    BigInt(String),
}

impl Literal {
    pub fn basic(&self) -> Basic {
        match self {
            Literal::String(_) => Basic::String,
            Literal::Boolean(_) => Basic::Boolean,
            Literal::Number(_) => Basic::Number,
            Literal::BigInt(_) => Basic::BigInt,
        }
    }
}

impl PartialEq for Literal {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Literal::String(a), Literal::String(b)) => a == b,
            (Literal::Boolean(a), Literal::Boolean(b)) => a == b,
            (Literal::Number(a), Literal::Number(b)) => a == b,
            (Literal::BigInt(a), Literal::BigInt(b)) => a == b,
            _ => false,
        }
    }
}

// Safe because the float literals don't include non-Eq values like NaN since they're literals.
impl Eq for Literal {}

impl Hash for Literal {
    fn hash<H: Hasher>(&self, state: &mut H) {
        fn integer_decode(val: f64) -> (u64, i16, i8) {
            let bits: u64 = val.to_bits();
            let sign: i8 = if bits >> 63 == 0 { 1 } else { -1 };
            let mut exponent: i16 = ((bits >> 52) & 0x7ff) as i16;
            let mantissa = if exponent == 0 {
                (bits & 0xfffffffffffff) << 1
            } else {
                (bits & 0xfffffffffffff) | 0x10000000000000
            };

            exponent -= 1023 + 52;
            (mantissa, exponent, sign)
        }

        match self {
            Literal::String(s) => s.hash(state),
            Literal::Boolean(b) => b.hash(state),
            Literal::Number(n) => {
                self.hash(state);
                integer_decode(*n).hash(state);
            }
            Literal::BigInt(s) => s.hash(state),
        }
    }
}

#[derive(Debug, Clone, Hash, Eq)]
pub struct Interface {
    /// Explicitly defined fields.
    pub fields: Vec<InterfaceField>,

    /// Set for index signature types, like `[key: string]: number`.
    pub index: Option<(Box<Type>, Box<Type>)>,

    /// Callable signature, like `(a: number): string`.
    /// The first tuple element is the args, and the second is the returns.
    pub call: Option<(Vec<Type>, Vec<Type>)>,
}

impl Interface {
    pub fn identical(&self, other: &Interface) -> bool {
        if self.fields.len() != other.fields.len() {
            return false;
        } else if self.index.is_some() != other.index.is_some() {
            return false;
        }

        // Collect the fields by name.
        let by_name = self
            .fields
            .iter()
            .map(|f| (f.name.clone(), f))
            .collect::<HashMap<_, _>>();

        // Check that all fields in `other` are in `self`.
        for field in &other.fields {
            if let Some(self_field) = by_name.get(&field.name) {
                if !self_field.identical(&field) {
                    return false;
                }
            } else {
                return false;
            }
        }

        // Compare index signatures.
        if let (Some((self_key, self_value)), Some((other_key, other_value))) =
            (&self.index, &other.index)
        {
            if !self_key.identical(other_key) || !self_value.identical(other_value) {
                return false;
            }
        }

        true
    }
}

impl PartialEq for Interface {
    fn eq(&self, other: &Self) -> bool {
        self.identical(other)
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct InterfaceField {
    pub name: String,
    pub typ: Type,
    pub optional: bool,
}

impl InterfaceField {
    pub fn identical(&self, other: &InterfaceField) -> bool {
        self.name == other.name && self.typ.identical(&other.typ) && self.optional == other.optional
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct ClassType {
    pub obj: Rc<object::Object>,
}

impl ClassType {
    pub fn identical(&self, _other: &ClassType) -> bool {
        todo!()
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct Named {
    pub obj: Rc<object::Object>,
    pub type_arguments: Vec<Type>,
}

impl Named {
    pub fn identical(&self, other: &Named) -> bool {
        if self.obj.id != other.obj.id || self.type_arguments.len() != other.type_arguments.len() {
            return false;
        }

        for (a, b) in self.type_arguments.iter().zip(&other.type_arguments) {
            if !a.identical(b) {
                return false;
            }
        }

        true
    }

    pub fn underlying(&self, ctx: &Ctx) -> Type {
        // TODO include type arguments
        ctx.obj_type(self.obj.clone())
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum Generic {
    /// A reference to a generic type parameter.
    TypeParam(TypeArgId),

    // TODO: include things like `T extends U` here
}

impl Generic {
    pub fn identical(&self, other: &Generic) -> bool {
        match (self, other) {
            (Generic::TypeParam(a), Generic::TypeParam(b)) => *a == *b,
        }
    }
}
