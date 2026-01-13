//! Pattern binding extraction for Python destructuring assignments.
//!
//! This module extracts variable bindings from Python assignment patterns,
//! tracking the path from the RHS value to each bound variable.
//!
//! # Examples
//!
//! ```python
//! # Simple assignment
//! x = value                    # x <- Full
//!
//! # Tuple unpacking
//! a, b = (1, 2)               # a <- TupleIndex(0), b <- TupleIndex(1)
//!
//! # Starred unpacking
//! first, *rest = [1, 2, 3]    # first <- TupleIndex(0), rest <- TupleRest(1)
//!
//! # Nested unpacking
//! (a, (b, c)) = (1, (2, 3))   # a <- TupleIndex(0)
//!                              # b <- TupleIndex(1), TupleIndex(0)
//!                              # c <- TupleIndex(1), TupleIndex(1)
//! ```

use ruff_python_ast as ast;
use ruff_text_size::TextRange;

/// A binding pattern extracted from a destructuring assignment.
#[derive(Debug, Clone)]
pub struct BindingPat {
    /// The variable name being bound.
    pub name: String,
    /// Source location of the binding.
    pub range: TextRange,
    /// Type annotation if present (from annotated assignment).
    pub annotation: Option<Box<ast::Expr>>,
    /// Default value if present (not common in Python bindings).
    pub default: Option<Box<ast::Expr>>,
    /// Path from the RHS value to this binding.
    pub destructure_path: Vec<DestructuringExpr>,
}

impl BindingPat {
    /// Create a new simple binding (no destructuring).
    pub fn simple(name: String, range: TextRange) -> Self {
        BindingPat {
            name,
            range,
            annotation: None,
            default: None,
            destructure_path: vec![DestructuringExpr::Full],
        }
    }

    /// Create a binding with a type annotation.
    pub fn with_annotation(mut self, annotation: Box<ast::Expr>) -> Self {
        self.annotation = Some(annotation);
        self
    }

    /// Create a binding with a default value.
    pub fn with_default(mut self, default: Box<ast::Expr>) -> Self {
        self.default = Some(default);
        self
    }

    /// Check if this binding uses destructuring.
    pub fn is_destructured(&self) -> bool {
        !matches!(
            self.destructure_path.as_slice(),
            [DestructuringExpr::Full] | []
        )
    }
}

/// A step in the destructuring path.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DestructuringExpr {
    /// The full value (no destructuring at this step).
    Full,
    /// Index into a tuple: `(a, b, c)[index]`
    TupleIndex(usize),
    /// Rest of a tuple after starred unpacking: `first, *rest = ...`
    TupleRest(usize),
    /// Index into a list: `[a, b, c][index]`
    ListIndex(usize),
    /// Rest of a list after starred unpacking.
    ListRest(usize),
    /// Key access on a dict-like object.
    DictKey(DestructuringKey),
    /// Rest of a dict (all keys except those extracted).
    DictRest { except: Vec<String> },
    /// Attribute access: `obj.attr`
    Attribute(String),
}

/// Key for dict destructuring.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DestructuringKey {
    /// String key: `{"key": value}`
    String(String),
    /// Integer key (for sequences accessed as mappings).
    Int(i64),
}

impl DestructuringKey {
    /// Get the string representation of the key.
    pub fn as_str(&self) -> String {
        match self {
            DestructuringKey::String(s) => s.clone(),
            DestructuringKey::Int(i) => i.to_string(),
        }
    }
}

/// Extract bindings from an assignment target pattern.
///
/// This function analyzes a Python assignment target and returns all variable
/// bindings with their destructuring paths.
pub fn extract_bindings(target: &ast::Expr) -> Vec<BindingPat> {
    let mut bindings = Vec::new();
    extract_bindings_impl(target, &mut Vec::new(), &mut bindings);
    bindings
}

fn extract_bindings_impl(
    target: &ast::Expr,
    path: &mut Vec<DestructuringExpr>,
    bindings: &mut Vec<BindingPat>,
) {
    match target {
        // Simple name binding
        ast::Expr::Name(name) => {
            let mut binding = BindingPat::simple(name.id.to_string(), name.range);
            if !path.is_empty() {
                binding.destructure_path = path.clone();
            }
            bindings.push(binding);
        }

        // Tuple unpacking: a, b, c = ...
        ast::Expr::Tuple(tuple) => {
            for (i, elt) in tuple.elts.iter().enumerate() {
                // Check for starred expression
                if let ast::Expr::Starred(starred) = elt {
                    path.push(DestructuringExpr::TupleRest(i));
                    extract_bindings_impl(&starred.value, path, bindings);
                    path.pop();
                } else {
                    path.push(DestructuringExpr::TupleIndex(i));
                    extract_bindings_impl(elt, path, bindings);
                    path.pop();
                }
            }
        }

        // List unpacking: [a, b, c] = ...
        ast::Expr::List(list) => {
            for (i, elt) in list.elts.iter().enumerate() {
                // Check for starred expression
                if let ast::Expr::Starred(starred) = elt {
                    path.push(DestructuringExpr::ListRest(i));
                    extract_bindings_impl(&starred.value, path, bindings);
                    path.pop();
                } else {
                    path.push(DestructuringExpr::ListIndex(i));
                    extract_bindings_impl(elt, path, bindings);
                    path.pop();
                }
            }
        }

        // Attribute access (uncommon as assignment target but valid)
        ast::Expr::Attribute(attr) => {
            // For attribute assignments like obj.attr = value,
            // we typically don't create a binding but record the path
            path.push(DestructuringExpr::Attribute(attr.attr.to_string()));
            // If the value is a name, it's an attribute of that name
            if let ast::Expr::Name(name) = attr.value.as_ref() {
                let binding = BindingPat {
                    name: format!("{}.{}", name.id, attr.attr),
                    range: attr.range,
                    annotation: None,
                    default: None,
                    destructure_path: path.clone(),
                };
                bindings.push(binding);
            }
            path.pop();
        }

        // Subscript access (uncommon as assignment target but valid)
        ast::Expr::Subscript(sub) => {
            // For subscript assignments like obj[key] = value
            if let Some(key) = extract_subscript_key(&sub.slice) {
                path.push(DestructuringExpr::DictKey(key));
            }
            // We don't typically create bindings for subscript targets
            if let ast::Expr::Name(name) = sub.value.as_ref() {
                let binding = BindingPat {
                    name: format!("{}[...]", name.id),
                    range: sub.range,
                    annotation: None,
                    default: None,
                    destructure_path: path.clone(),
                };
                bindings.push(binding);
            }
            if !path.is_empty() {
                path.pop();
            }
        }

        // Starred expression (handled in tuple/list context above)
        ast::Expr::Starred(starred) => {
            extract_bindings_impl(&starred.value, path, bindings);
        }

        // Other expressions are not valid assignment targets in standard Python
        _ => {}
    }
}

/// Try to extract a key from a subscript slice.
fn extract_subscript_key(slice: &ast::Expr) -> Option<DestructuringKey> {
    match slice {
        ast::Expr::StringLiteral(s) => {
            Some(DestructuringKey::String(s.value.to_string()))
        }
        ast::Expr::NumberLiteral(n) => {
            if let ast::Number::Int(i) = &n.value {
                i.as_i64().map(DestructuringKey::Int)
            } else {
                None
            }
        }
        _ => None,
    }
}

/// Extract bindings from a function parameter list.
///
/// This handles all Python parameter kinds:
/// - Regular positional/keyword parameters
/// - Positional-only parameters (before /)
/// - Keyword-only parameters (after *)
/// - *args and **kwargs
pub fn extract_param_bindings(params: &ast::Parameters) -> Vec<ParamBinding> {
    let mut bindings = Vec::new();

    // Positional-only parameters
    for param in &params.posonlyargs {
        bindings.push(ParamBinding {
            name: param.parameter.name.to_string(),
            range: param.parameter.range,
            annotation: param.parameter.annotation.clone(),
            default: param.default.clone(),
            kind: ParamKind::PositionalOnly,
        });
    }

    // Regular parameters (positional or keyword)
    for param in &params.args {
        bindings.push(ParamBinding {
            name: param.parameter.name.to_string(),
            range: param.parameter.range,
            annotation: param.parameter.annotation.clone(),
            default: param.default.clone(),
            kind: ParamKind::Regular,
        });
    }

    // *args parameter
    if let Some(vararg) = &params.vararg {
        bindings.push(ParamBinding {
            name: vararg.name.to_string(),
            range: vararg.range,
            annotation: vararg.annotation.clone(),
            default: None,
            kind: ParamKind::Vararg,
        });
    }

    // Keyword-only parameters
    for param in &params.kwonlyargs {
        bindings.push(ParamBinding {
            name: param.parameter.name.to_string(),
            range: param.parameter.range,
            annotation: param.parameter.annotation.clone(),
            default: param.default.clone(),
            kind: ParamKind::KeywordOnly,
        });
    }

    // **kwargs parameter
    if let Some(kwarg) = &params.kwarg {
        bindings.push(ParamBinding {
            name: kwarg.name.to_string(),
            range: kwarg.range,
            annotation: kwarg.annotation.clone(),
            default: None,
            kind: ParamKind::Kwarg,
        });
    }

    bindings
}

/// A function parameter binding.
#[derive(Debug, Clone)]
pub struct ParamBinding {
    /// Parameter name.
    pub name: String,
    /// Source location.
    pub range: TextRange,
    /// Type annotation if present.
    pub annotation: Option<Box<ast::Expr>>,
    /// Default value if present.
    pub default: Option<Box<ast::Expr>>,
    /// Parameter kind.
    pub kind: ParamKind,
}

/// Kind of function parameter.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ParamKind {
    /// Regular positional or keyword parameter.
    Regular,
    /// Positional-only parameter (before `/`).
    PositionalOnly,
    /// Keyword-only parameter (after `*`).
    KeywordOnly,
    /// *args parameter.
    Vararg,
    /// **kwargs parameter.
    Kwarg,
}

#[cfg(test)]
mod tests {
    use super::*;

    // Note: These tests would require parsing actual Python code.
    // In a real implementation, you'd use ruff_python_parser to create test AST nodes.

    #[test]
    fn test_destructuring_key() {
        let key = DestructuringKey::String("test".to_string());
        assert_eq!(key.as_str(), "test");

        let key = DestructuringKey::Int(42);
        assert_eq!(key.as_str(), "42");
    }

    #[test]
    fn test_binding_pat_simple() {
        let binding = BindingPat::simple("x".to_string(), TextRange::default());
        assert!(!binding.is_destructured());
        assert_eq!(binding.name, "x");
    }

    #[test]
    fn test_param_kind() {
        assert_eq!(ParamKind::Regular, ParamKind::Regular);
        assert_ne!(ParamKind::Regular, ParamKind::Vararg);
    }
}
