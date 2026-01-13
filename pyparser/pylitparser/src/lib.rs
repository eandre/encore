//! Python literal parser library for parsing Python AST literals into Rust types.
//!
//! This library provides a trait-based approach for parsing Python dictionary literals
//! and other literal expressions into strongly-typed Rust structures.

use duration_string::DurationString;
use either::Either;
use num_bigint::{BigInt, ToBigInt};
use ruff_python_ast as ast;
use ruff_text_size::{Ranged, TextRange};
use std::{
    error::Error,
    fmt::{Debug, Display},
    ops::{Deref, DerefMut},
    path::{Component, PathBuf},
};

/// A parse error with source location information.
#[derive(Debug, Clone, Hash)]
pub struct ParseError {
    pub range: TextRange,
    pub message: String,
}

impl Error for ParseError {}

impl Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.message)
    }
}

/// Trait for converting a ranged type into a parse error.
pub trait ToParseErr {
    fn parse_err<S: Into<String>>(&self, message: S) -> ParseError;
}

impl<T> ToParseErr for T
where
    T: Ranged,
{
    fn parse_err<S: Into<String>>(&self, message: S) -> ParseError {
        ParseError {
            range: self.range(),
            message: message.into(),
        }
    }
}

/// Result type for parsing operations.
pub type ParseResult<T> = Result<T, ParseError>;

/// Trait for parsing Python AST literals into Rust types.
///
/// The lifetime parameter `'py` represents the lifetime of the input AST,
/// allowing parsed types to borrow from the input expression.
pub trait LitParser<'py>: Sized {
    fn parse_lit(input: &'py ast::Expr) -> ParseResult<Self>;
}

/// A span wrapper that pairs a TextRange with a value.
pub struct Sp<T>(TextRange, T);

impl<T> Clone for Sp<T>
where
    T: Clone,
{
    fn clone(&self) -> Self {
        Sp(self.0, self.1.clone())
    }
}

impl<T> Sp<T> {
    pub fn new(range: TextRange, val: T) -> Self {
        Self(range, val)
    }

    pub fn with_dummy(val: T) -> Self {
        Self::new(TextRange::default(), val)
    }

    pub fn with<U>(&self, val: U) -> Sp<U> {
        Sp::new(self.0, val)
    }

    pub fn split(self) -> (TextRange, T) {
        (self.0, self.1)
    }

    pub fn text_range(&self) -> TextRange {
        self.0
    }

    pub fn take(self) -> T {
        self.1
    }

    pub fn map<F, U>(self, f: F) -> Sp<U>
    where
        F: FnOnce(T) -> U,
    {
        Sp(self.0, f(self.1))
    }

    pub fn get(&self) -> &T {
        &self.1
    }

    pub fn as_deref(&self) -> &T::Target
    where
        T: Deref,
    {
        self.1.deref()
    }
}

impl<T, E> Sp<Result<T, E>> {
    pub fn transpose(self) -> Result<Sp<T>, E> {
        match self.1 {
            Ok(inner) => Ok(Sp(self.0, inner)),
            Err(err) => Err(err),
        }
    }
}

impl<T> AsRef<T> for Sp<T> {
    fn as_ref(&self) -> &T {
        &self.1
    }
}

impl<T> AsMut<T> for Sp<T> {
    fn as_mut(&mut self) -> &mut T {
        &mut self.1
    }
}

impl<T> Deref for Sp<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.1
    }
}

impl<T> DerefMut for Sp<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.1
    }
}

impl<T> PartialEq for Sp<T>
where
    T: PartialEq,
{
    fn eq(&self, other: &Self) -> bool {
        self.1 == other.1
    }
}

impl<T> Eq for Sp<T> where T: Eq {}

impl<T> PartialOrd for Sp<T>
where
    T: PartialOrd,
{
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.1.partial_cmp(&other.1)
    }
}

impl<T> Ord for Sp<T>
where
    T: Ord,
{
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.1.cmp(&other.1)
    }
}

impl<T> Copy for Sp<T> where T: Copy {}

impl<T> Debug for Sp<T>
where
    T: Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.1.fmt(f)
    }
}

impl<T> Display for Sp<T>
where
    T: Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.1.fmt(f)
    }
}

impl<T> Ranged for Sp<T> {
    fn range(&self) -> TextRange {
        self.0
    }
}

// LitParser implementations

impl<'py, T> LitParser<'py> for Sp<T>
where
    T: LitParser<'py>,
{
    fn parse_lit(input: &'py ast::Expr) -> ParseResult<Self> {
        let res = T::parse_lit(input)?;
        Ok(Sp(input.range(), res))
    }
}

impl<'py> LitParser<'py> for String {
    fn parse_lit(input: &'py ast::Expr) -> ParseResult<Self> {
        match input {
            ast::Expr::StringLiteral(lit) => Ok(lit.value.to_string()),
            _ => Err(input.parse_err("expected string literal")),
        }
    }
}

impl<'py> LitParser<'py> for bool {
    fn parse_lit(input: &'py ast::Expr) -> ParseResult<Self> {
        match input {
            ast::Expr::BooleanLiteral(b) => Ok(b.value),
            _ => Err(input.parse_err("expected boolean literal")),
        }
    }
}

impl<'py> LitParser<'py> for i32 {
    fn parse_lit(input: &'py ast::Expr) -> ParseResult<Self> {
        let big = parse_const_bigint(input)?;
        big.try_into()
            .map_err(|_| input.parse_err("expected number literal"))
    }
}

impl<'py> LitParser<'py> for u32 {
    fn parse_lit(input: &'py ast::Expr) -> ParseResult<Self> {
        let big = parse_const_bigint(input)?;
        big.try_into()
            .map_err(|_| input.parse_err("expected unsigned number literal"))
    }
}

impl<'py> LitParser<'py> for i64 {
    fn parse_lit(input: &'py ast::Expr) -> ParseResult<Self> {
        let big = parse_const_bigint(input)?;
        big.try_into()
            .map_err(|_| input.parse_err("expected number literal"))
    }
}

impl<'py> LitParser<'py> for u64 {
    fn parse_lit(input: &'py ast::Expr) -> ParseResult<Self> {
        let big = parse_const_bigint(input)?;
        big.try_into()
            .map_err(|_| input.parse_err("expected unsigned number literal"))
    }
}

impl<'py> LitParser<'py> for ast::Expr {
    fn parse_lit(input: &'py ast::Expr) -> ParseResult<Self> {
        Ok(input.clone())
    }
}

impl<'py> LitParser<'py> for &'py ast::Expr {
    fn parse_lit(input: &'py ast::Expr) -> ParseResult<Self> {
        Ok(input)
    }
}

impl<'py, T> LitParser<'py> for Option<T>
where
    T: LitParser<'py>,
{
    fn parse_lit(input: &'py ast::Expr) -> ParseResult<Option<T>> {
        let t = T::parse_lit(input)?;
        Ok(Some(t))
    }
}

impl<'py, L, R> LitParser<'py> for Either<L, R>
where
    L: LitParser<'py>,
    R: LitParser<'py>,
{
    fn parse_lit(input: &'py ast::Expr) -> ParseResult<Either<L, R>> {
        let res = L::parse_lit(input)
            .map(Either::Left)
            .or_else(|_| R::parse_lit(input).map(Either::Right))?;

        Ok(res)
    }
}

impl<'py> LitParser<'py> for std::time::Duration {
    fn parse_lit(input: &'py ast::Expr) -> ParseResult<Self> {
        match input {
            ast::Expr::StringLiteral(lit) => {
                let dur = DurationString::try_from(lit.value.to_string())
                    .map_err(|e| input.parse_err(e))?;
                Ok(dur.into())
            }
            _ => Err(input.parse_err("expected duration string literal")),
        }
    }
}

impl<'py, T> LitParser<'py> for Vec<T>
where
    T: LitParser<'py>,
{
    fn parse_lit(input: &'py ast::Expr) -> ParseResult<Self> {
        match input {
            ast::Expr::List(list) => {
                let mut vec = Vec::new();
                for elem in &list.elts {
                    let parsed_elem = T::parse_lit(elem)?;
                    vec.push(parsed_elem);
                }
                Ok(vec)
            }
            _ => Err(input.parse_err("expected list literal")),
        }
    }
}

/// Represents a local, relative path (without ".." or a root).
#[derive(Debug, Clone)]
pub struct LocalRelPath {
    pub range: TextRange,
    pub buf: PathBuf,
}

impl Ranged for LocalRelPath {
    fn range(&self) -> TextRange {
        self.range
    }
}

impl LocalRelPath {
    pub fn try_from<S: AsRef<str>>(range: TextRange, str: S) -> ParseResult<Self> {
        let str = str.as_ref();
        let path = PathBuf::from(str);
        for c in path.components() {
            match c {
                Component::CurDir => {}
                Component::Normal(_) => {}
                _ => return Err(range.parse_err("expected a local relative path")),
            }
        }
        Ok(LocalRelPath {
            range,
            buf: clean_path::clean(path),
        })
    }
}

impl<'py> LitParser<'py> for LocalRelPath {
    fn parse_lit(input: &'py ast::Expr) -> ParseResult<Self> {
        match input {
            ast::Expr::StringLiteral(lit) => {
                LocalRelPath::try_from(input.range(), lit.value.to_string())
            }
            _ => Err(input.parse_err("expected a local relative path")),
        }
    }
}

/// Nullable wrapper for distinguishing between missing fields and explicit None.
#[derive(Debug)]
pub enum Nullable<T> {
    Present(T),
    Null,
}

impl<'py, T> LitParser<'py> for Nullable<T>
where
    T: LitParser<'py>,
{
    fn parse_lit(input: &'py ast::Expr) -> ParseResult<Self> {
        match input {
            ast::Expr::NoneLiteral(_) => Ok(Nullable::Null),
            _ => {
                let t = T::parse_lit(input)?;
                Ok(Nullable::Present(t))
            }
        }
    }
}

impl<T> Clone for Nullable<T>
where
    T: Clone,
{
    fn clone(&self) -> Self {
        match self {
            Nullable::Present(t) => Nullable::Present(t.clone()),
            Nullable::Null => Nullable::Null,
        }
    }
}

/// Parses a constant integer expression with support for unary and binary operators.
fn parse_const_bigint(expr: &ast::Expr) -> ParseResult<BigInt> {
    match expr {
        ast::Expr::NumberLiteral(num) => match &num.value {
            ast::Number::Int(int) => {
                let Some(big) = int.as_i64().and_then(|i| i.to_bigint()) else {
                    // Try to get it as a BigInt directly if it's too large for i64
                    return Err(expr.parse_err("integer too large"));
                };
                Ok(big)
            }
            ast::Number::Float(_) => Err(expr.parse_err("expected integer literal, got float")),
            ast::Number::Complex { .. } => {
                Err(expr.parse_err("expected integer literal, got complex"))
            }
        },
        ast::Expr::UnaryOp(unary) => match unary.op {
            ast::UnaryOp::USub => {
                let x = parse_const_bigint(&unary.operand)?;
                Ok(-x)
            }
            ast::UnaryOp::UAdd => parse_const_bigint(&unary.operand),
            _ => Err(expr.parse_err(format!("unsupported unary operator {:?}", unary.op))),
        },
        ast::Expr::BinOp(bin) => {
            let x = parse_const_bigint(&bin.left)?;
            let y = parse_const_bigint(&bin.right)?;
            match bin.op {
                ast::Operator::Add => Ok(x + y),
                ast::Operator::Sub => Ok(x - y),
                ast::Operator::Mult => Ok(x * y),
                ast::Operator::Mod => Ok(x % y),
                ast::Operator::Div | ast::Operator::FloorDiv => {
                    // Does it divide evenly?
                    use num_integer::Integer;
                    use num_traits::Zero;
                    let (quo, remainder) = x.div_rem(&y);
                    if remainder.is_zero() {
                        Ok(quo)
                    } else {
                        Err(expr.parse_err("expected integer division"))
                    }
                }
                _ => Err(expr.parse_err(format!(
                    "expected arithmetic operator, got {:?}",
                    bin.op
                ))),
            }
        }
        _ => Err(expr.parse_err("expected integer literal")),
    }
}

/// Macro for reporting an error and continuing.
#[macro_export]
macro_rules! report_and_continue {
    ($e:expr) => {
        match $e {
            Ok(v) => v,
            Err(err) => {
                eprintln!("Parse error: {}", err);
                continue;
            }
        }
    };
}

/// Macro for reporting an error and returning.
#[macro_export]
macro_rules! report_and_return {
    ($e:expr) => {
        match $e {
            Ok(v) => v,
            Err(err) => {
                eprintln!("Parse error: {}", err);
                return;
            }
        }
    };
}

#[cfg(test)]
mod tests {
    use super::*;
    use ruff_python_parser::parse_expression;

    fn parse_expr(src: &str) -> ast::Expr {
        parse_expression(src)
            .expect("failed to parse expression")
            .into_expr()
    }

    #[test]
    fn test_parse_string() {
        let expr = parse_expr(r#""hello""#);
        let result: String = LitParser::parse_lit(&expr).expect("failed to parse");
        assert_eq!(result, "hello");
    }

    #[test]
    fn test_parse_bool() {
        let expr = parse_expr("True");
        let result: bool = LitParser::parse_lit(&expr).expect("failed to parse");
        assert!(result);

        let expr = parse_expr("False");
        let result: bool = LitParser::parse_lit(&expr).expect("failed to parse");
        assert!(!result);
    }

    #[test]
    fn test_parse_int() {
        let expr = parse_expr("42");
        let result: i32 = LitParser::parse_lit(&expr).expect("failed to parse");
        assert_eq!(result, 42);

        let expr = parse_expr("-10");
        let result: i32 = LitParser::parse_lit(&expr).expect("failed to parse");
        assert_eq!(result, -10);
    }

    #[test]
    fn test_parse_duration() {
        let expr = parse_expr(r#""1h""#);
        let result: std::time::Duration = LitParser::parse_lit(&expr).expect("failed to parse");
        assert_eq!(result, std::time::Duration::from_secs(3600));
    }

    #[test]
    fn test_parse_list() {
        let expr = parse_expr(r#"["a", "b", "c"]"#);
        let result: Vec<String> = LitParser::parse_lit(&expr).expect("failed to parse");
        assert_eq!(result, vec!["a", "b", "c"]);
    }

    #[test]
    fn test_parse_nullable() {
        let expr = parse_expr("None");
        let result: Nullable<String> = LitParser::parse_lit(&expr).expect("failed to parse");
        assert!(matches!(result, Nullable::Null));

        let expr = parse_expr(r#""hello""#);
        let result: Nullable<String> = LitParser::parse_lit(&expr).expect("failed to parse");
        match result {
            Nullable::Present(s) => assert_eq!(s, "hello"),
            Nullable::Null => panic!("expected Present"),
        }
    }
}
