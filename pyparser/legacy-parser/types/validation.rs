//! Validation expressions for Python type constraints.
//!
//! This module provides a way to represent runtime validation constraints
//! similar to pydantic's Field validators, Annotated metadata, and custom validators.
//!
//! # Example
//!
//! Python code like:
//! ```python
//! from pydantic import Field
//! from typing import Annotated
//!
//! name: Annotated[str, Field(min_length=1, max_length=100)]
//! age: Annotated[int, Field(ge=0, le=150)]
//! email: Annotated[str, Field(pattern=r'^[\w\.-]+@[\w\.-]+\.\w+$')]
//! ```
//!
//! Gets represented as validation expressions that can be:
//! - Combined with AND/OR logic
//! - Simplified and optimized
//! - Validated against the underlying type
//! - Serialized for code generation

use super::typ::{Basic, Type};

/// A validation expression tree.
///
/// Expressions can be combined with AND/OR logic to create complex
/// validation rules.
#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    /// A single validation rule.
    Rule(Rule),
    /// Logical AND of multiple expressions (all must pass).
    And(Vec<Expr>),
    /// Logical OR of multiple expressions (at least one must pass).
    Or(Vec<Expr>),
}

impl Expr {
    /// Create an AND expression from two expressions.
    pub fn and(self, other: Self) -> Self {
        match (self, other) {
            (Expr::And(mut a), Expr::And(b)) => {
                a.extend(b);
                Expr::And(a)
            }
            (Expr::And(mut a), other) => {
                a.push(other);
                Expr::And(a)
            }
            (other, Expr::And(mut a)) => {
                a.insert(0, other);
                Expr::And(a)
            }
            (a, b) => Expr::And(vec![a, b]),
        }
    }

    /// Create an OR expression from two expressions.
    pub fn or(self, other: Self) -> Self {
        match (self, other) {
            (Expr::Or(mut a), Expr::Or(b)) => {
                a.extend(b);
                Expr::Or(a)
            }
            (Expr::Or(mut a), other) => {
                a.push(other);
                Expr::Or(a)
            }
            (other, Expr::Or(mut a)) => {
                a.insert(0, other);
                Expr::Or(a)
            }
            (a, b) => Expr::Or(vec![a, b]),
        }
    }

    /// Simplify the expression by merging compatible rules.
    pub fn simplify(self) -> Self {
        match self {
            Expr::Rule(_) => self,
            Expr::And(exprs) => {
                let mut simplified: Vec<Expr> = Vec::new();
                for expr in exprs {
                    let expr = expr.simplify();
                    // Try to merge with existing rules
                    let mut merged = false;
                    if let Expr::Rule(ref new_rule) = expr {
                        for existing in simplified.iter_mut() {
                            if let Expr::Rule(ref mut existing_rule) = existing {
                                if let Some(merged_rule) = existing_rule.merge_and(new_rule) {
                                    *existing_rule = merged_rule;
                                    merged = true;
                                    break;
                                }
                            }
                        }
                    }
                    if !merged {
                        simplified.push(expr);
                    }
                }
                if simplified.len() == 1 {
                    simplified.pop().unwrap()
                } else {
                    Expr::And(simplified)
                }
            }
            Expr::Or(exprs) => {
                let mut simplified: Vec<Expr> = Vec::new();
                for expr in exprs {
                    let expr = expr.simplify();
                    // Try to merge with existing rules
                    let mut merged = false;
                    if let Expr::Rule(ref new_rule) = expr {
                        for existing in simplified.iter_mut() {
                            if let Expr::Rule(ref mut existing_rule) = existing {
                                if let Some(merged_rule) = existing_rule.merge_or(new_rule) {
                                    *existing_rule = merged_rule;
                                    merged = true;
                                    break;
                                }
                            }
                        }
                    }
                    if !merged {
                        simplified.push(expr);
                    }
                }
                if simplified.len() == 1 {
                    simplified.pop().unwrap()
                } else {
                    Expr::Or(simplified)
                }
            }
        }
    }

    /// Check if this validation expression is compatible with the given type.
    pub fn supports_type(&self, typ: &Type) -> Result<(), UnsupportedValidationsError> {
        let mut unsupported = Vec::new();
        self.collect_unsupported(typ, &mut unsupported);
        if unsupported.is_empty() {
            Ok(())
        } else {
            Err(UnsupportedValidationsError { rules: unsupported })
        }
    }

    fn collect_unsupported(&self, typ: &Type, unsupported: &mut Vec<Rule>) {
        match self {
            Expr::Rule(rule) => {
                if !rule.supports_type(typ) {
                    unsupported.push(rule.clone());
                }
            }
            Expr::And(exprs) | Expr::Or(exprs) => {
                for expr in exprs {
                    expr.collect_unsupported(typ, unsupported);
                }
            }
        }
    }

    /// Get all rules in this expression (flattened).
    pub fn rules(&self) -> Vec<&Rule> {
        match self {
            Expr::Rule(rule) => vec![rule],
            Expr::And(exprs) | Expr::Or(exprs) => exprs.iter().flat_map(|e| e.rules()).collect(),
        }
    }

    /// Check if the expression is empty (no rules).
    pub fn is_empty(&self) -> bool {
        match self {
            Expr::Rule(_) => false,
            Expr::And(exprs) | Expr::Or(exprs) => exprs.is_empty(),
        }
    }
}

impl Default for Expr {
    fn default() -> Self {
        Expr::And(Vec::new())
    }
}

/// A single validation rule.
#[derive(Debug, Clone, PartialEq)]
pub enum Rule {
    // String length constraints
    /// Minimum string length: `min_length=N`
    MinLen(u64),
    /// Maximum string length: `max_length=N`
    MaxLen(u64),

    // Numeric constraints
    /// Greater than: `gt=N`
    Gt(N),
    /// Greater than or equal: `ge=N`
    Ge(N),
    /// Less than: `lt=N`
    Lt(N),
    /// Less than or equal: `le=N`
    Le(N),
    /// Multiple of: `multiple_of=N`
    MultipleOf(N),

    // String pattern constraints
    /// String starts with: `pattern='^prefix'`
    StartsWith(String),
    /// String ends with: `pattern='suffix$'`
    EndsWith(String),
    /// Regex pattern match: `pattern=r'...'`
    Pattern(String),

    // Collection constraints
    /// Minimum collection items: `min_length=N` for list/set
    MinItems(u64),
    /// Maximum collection items: `max_length=N` for list/set
    MaxItems(u64),
    /// All items must be unique (set semantics)
    UniqueItems,

    // Special type constraints
    /// Must be a specific format
    Is(Is),

    // Custom validators
    /// Custom validation function reference
    Custom(CustomValidator),
}

/// Numeric value for constraints (supports int and float).
#[derive(Debug, Clone, PartialEq)]
pub enum N {
    /// Integer value.
    Int(i64),
    /// Float value.
    Float(f64),
}

impl N {
    /// Compare two N values.
    pub fn cmp(&self, other: &N) -> Option<std::cmp::Ordering> {
        match (self, other) {
            (N::Int(a), N::Int(b)) => Some(a.cmp(b)),
            (N::Float(a), N::Float(b)) => a.partial_cmp(b),
            (N::Int(a), N::Float(b)) => (*a as f64).partial_cmp(b),
            (N::Float(a), N::Int(b)) => a.partial_cmp(&(*b as f64)),
        }
    }

    /// Get the maximum of two N values.
    pub fn max(self, other: N) -> N {
        if self.cmp(&other) == Some(std::cmp::Ordering::Greater) {
            self
        } else {
            other
        }
    }

    /// Get the minimum of two N values.
    pub fn min(self, other: N) -> N {
        if self.cmp(&other) == Some(std::cmp::Ordering::Less) {
            self
        } else {
            other
        }
    }
}

/// Special format validators.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Is {
    /// Valid email address.
    Email,
    /// Valid URL.
    Url,
    /// Valid UUID.
    Uuid,
    /// Valid IPv4 address.
    IPv4,
    /// Valid IPv6 address.
    IPv6,
    /// Valid IP address (v4 or v6).
    IP,
    /// Valid hostname.
    Hostname,
    /// Valid date (ISO 8601).
    Date,
    /// Valid time (ISO 8601).
    Time,
    /// Valid datetime (ISO 8601).
    DateTime,
    /// Valid duration (ISO 8601).
    Duration,
    /// Valid JSON.
    Json,
    /// Valid base64.
    Base64,
    /// Valid credit card number (Luhn check).
    CreditCard,
    /// Valid phone number (E.164).
    PhoneNumber,
}

impl Is {
    /// Get the name of this format validator.
    pub fn name(&self) -> &'static str {
        match self {
            Is::Email => "email",
            Is::Url => "url",
            Is::Uuid => "uuid",
            Is::IPv4 => "ipv4",
            Is::IPv6 => "ipv6",
            Is::IP => "ip",
            Is::Hostname => "hostname",
            Is::Date => "date",
            Is::Time => "time",
            Is::DateTime => "datetime",
            Is::Duration => "duration",
            Is::Json => "json",
            Is::Base64 => "base64",
            Is::CreditCard => "credit_card",
            Is::PhoneNumber => "phone_number",
        }
    }

    /// Get the underlying type required for this format.
    pub fn required_type(&self) -> Basic {
        Basic::Str
    }
}

/// Custom validator reference.
#[derive(Debug, Clone, PartialEq)]
pub struct CustomValidator {
    /// Validator function name or path.
    pub name: String,
    /// Module where the validator is defined.
    pub module: Option<String>,
    /// Whether this is a field validator or model validator.
    pub kind: CustomValidatorKind,
}

/// Kind of custom validator.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CustomValidatorKind {
    /// Field validator (validates a single field).
    Field,
    /// Model validator (validates the entire model).
    Model,
    /// Wrap validator (wraps the default validation).
    Wrap,
    /// Before validator (runs before type coercion).
    Before,
    /// After validator (runs after type coercion).
    After,
}

impl Rule {
    /// Try to merge two rules with AND logic.
    ///
    /// Returns Some if the rules can be merged, None otherwise.
    pub fn merge_and(&self, other: &Self) -> Option<Self> {
        match (self, other) {
            // Length constraints: take stricter
            (Rule::MinLen(a), Rule::MinLen(b)) => Some(Rule::MinLen((*a).max(*b))),
            (Rule::MaxLen(a), Rule::MaxLen(b)) => Some(Rule::MaxLen((*a).min(*b))),
            (Rule::MinItems(a), Rule::MinItems(b)) => Some(Rule::MinItems((*a).max(*b))),
            (Rule::MaxItems(a), Rule::MaxItems(b)) => Some(Rule::MaxItems((*a).min(*b))),

            // Numeric constraints: take stricter
            (Rule::Gt(a), Rule::Gt(b)) => Some(Rule::Gt(a.clone().max(b.clone()))),
            (Rule::Ge(a), Rule::Ge(b)) => Some(Rule::Ge(a.clone().max(b.clone()))),
            (Rule::Lt(a), Rule::Lt(b)) => Some(Rule::Lt(a.clone().min(b.clone()))),
            (Rule::Le(a), Rule::Le(b)) => Some(Rule::Le(a.clone().min(b.clone()))),

            // UniqueItems is idempotent
            (Rule::UniqueItems, Rule::UniqueItems) => Some(Rule::UniqueItems),

            _ => None,
        }
    }

    /// Try to merge two rules with OR logic.
    ///
    /// Returns Some if the rules can be merged, None otherwise.
    pub fn merge_or(&self, other: &Self) -> Option<Self> {
        match (self, other) {
            // Length constraints: take looser
            (Rule::MinLen(a), Rule::MinLen(b)) => Some(Rule::MinLen((*a).min(*b))),
            (Rule::MaxLen(a), Rule::MaxLen(b)) => Some(Rule::MaxLen((*a).max(*b))),
            (Rule::MinItems(a), Rule::MinItems(b)) => Some(Rule::MinItems((*a).min(*b))),
            (Rule::MaxItems(a), Rule::MaxItems(b)) => Some(Rule::MaxItems((*a).max(*b))),

            // Numeric constraints: take looser
            (Rule::Gt(a), Rule::Gt(b)) => Some(Rule::Gt(a.clone().min(b.clone()))),
            (Rule::Ge(a), Rule::Ge(b)) => Some(Rule::Ge(a.clone().min(b.clone()))),
            (Rule::Lt(a), Rule::Lt(b)) => Some(Rule::Lt(a.clone().max(b.clone()))),
            (Rule::Le(a), Rule::Le(b)) => Some(Rule::Le(a.clone().max(b.clone()))),

            _ => None,
        }
    }

    /// Check if this rule is compatible with the given type.
    pub fn supports_type(&self, typ: &Type) -> bool {
        match self {
            // String length rules
            Rule::MinLen(_) | Rule::MaxLen(_) => matches!(
                typ,
                Type::Basic(Basic::Str) | Type::Basic(Basic::Bytes) | Type::Basic(Basic::ByteArray)
            ),

            // String pattern rules
            Rule::StartsWith(_) | Rule::EndsWith(_) | Rule::Pattern(_) => {
                matches!(typ, Type::Basic(Basic::Str))
            }

            // Numeric rules
            Rule::Gt(_) | Rule::Ge(_) | Rule::Lt(_) | Rule::Le(_) | Rule::MultipleOf(_) => {
                matches!(
                    typ,
                    Type::Basic(Basic::Int)
                        | Type::Basic(Basic::Float)
                        | Type::Basic(Basic::Complex)
                        | Type::Custom(super::typ::Custom::Decimal)
                )
            }

            // Collection rules
            Rule::MinItems(_) | Rule::MaxItems(_) | Rule::UniqueItems => {
                matches!(
                    typ,
                    Type::List(_) | Type::Set(_) | Type::FrozenSet(_) | Type::Tuple(_, _)
                )
            }

            // Format rules - mostly strings
            Rule::Is(is) => match is {
                Is::Email
                | Is::Url
                | Is::Uuid
                | Is::IPv4
                | Is::IPv6
                | Is::IP
                | Is::Hostname
                | Is::Json
                | Is::Base64
                | Is::CreditCard
                | Is::PhoneNumber => matches!(typ, Type::Basic(Basic::Str)),
                Is::Date | Is::Time | Is::DateTime | Is::Duration => {
                    matches!(
                        typ,
                        Type::Basic(Basic::Str)
                            | Type::Custom(super::typ::Custom::Date)
                            | Type::Custom(super::typ::Custom::Time)
                            | Type::Custom(super::typ::Custom::DateTime)
                            | Type::Custom(super::typ::Custom::TimeDelta)
                    )
                }
            },

            // Custom validators can apply to any type
            Rule::Custom(_) => true,
        }
    }

    /// Get a human-readable description of this rule.
    pub fn description(&self) -> String {
        match self {
            Rule::MinLen(n) => format!("minimum length {}", n),
            Rule::MaxLen(n) => format!("maximum length {}", n),
            Rule::Gt(n) => format!("greater than {:?}", n),
            Rule::Ge(n) => format!("greater than or equal to {:?}", n),
            Rule::Lt(n) => format!("less than {:?}", n),
            Rule::Le(n) => format!("less than or equal to {:?}", n),
            Rule::MultipleOf(n) => format!("multiple of {:?}", n),
            Rule::StartsWith(s) => format!("starts with {:?}", s),
            Rule::EndsWith(s) => format!("ends with {:?}", s),
            Rule::Pattern(p) => format!("matches pattern {:?}", p),
            Rule::MinItems(n) => format!("minimum {} items", n),
            Rule::MaxItems(n) => format!("maximum {} items", n),
            Rule::UniqueItems => "unique items".to_string(),
            Rule::Is(is) => format!("valid {}", is.name()),
            Rule::Custom(c) => format!("custom validator: {}", c.name),
        }
    }
}

/// Error returned when validation rules don't match the type.
#[derive(Debug)]
pub struct UnsupportedValidationsError {
    /// The rules that don't support the type.
    pub rules: Vec<Rule>,
}

impl std::fmt::Display for UnsupportedValidationsError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "unsupported validation rules: ")?;
        for (i, rule) in self.rules.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{}", rule.description())?;
        }
        Ok(())
    }
}

impl std::error::Error for UnsupportedValidationsError {}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_expr_and() {
        let a = Expr::Rule(Rule::MinLen(5));
        let b = Expr::Rule(Rule::MaxLen(10));
        let combined = a.and(b);

        if let Expr::And(exprs) = combined {
            assert_eq!(exprs.len(), 2);
        } else {
            panic!("Expected And expression");
        }
    }

    #[test]
    fn test_expr_simplify_and() {
        let a = Expr::Rule(Rule::MinLen(5));
        let b = Expr::Rule(Rule::MinLen(10));
        let combined = a.and(b).simplify();

        // Should merge to MinLen(10) (the stricter one)
        assert_eq!(combined, Expr::Rule(Rule::MinLen(10)));
    }

    #[test]
    fn test_expr_simplify_or() {
        let a = Expr::Rule(Rule::MinLen(5));
        let b = Expr::Rule(Rule::MinLen(10));
        let combined = a.or(b).simplify();

        // Should merge to MinLen(5) (the looser one)
        assert_eq!(combined, Expr::Rule(Rule::MinLen(5)));
    }

    #[test]
    fn test_rule_supports_type() {
        assert!(Rule::MinLen(5).supports_type(&Type::Basic(Basic::Str)));
        assert!(!Rule::MinLen(5).supports_type(&Type::Basic(Basic::Int)));

        assert!(Rule::Ge(N::Int(0)).supports_type(&Type::Basic(Basic::Int)));
        assert!(Rule::Ge(N::Float(0.0)).supports_type(&Type::Basic(Basic::Float)));
        assert!(!Rule::Ge(N::Int(0)).supports_type(&Type::Basic(Basic::Str)));
    }

    #[test]
    fn test_n_comparison() {
        assert_eq!(N::Int(5).cmp(&N::Int(10)), Some(std::cmp::Ordering::Less));
        assert_eq!(
            N::Float(5.5).cmp(&N::Float(5.5)),
            Some(std::cmp::Ordering::Equal)
        );
        assert_eq!(
            N::Int(5).cmp(&N::Float(5.5)),
            Some(std::cmp::Ordering::Less)
        );
    }
}
