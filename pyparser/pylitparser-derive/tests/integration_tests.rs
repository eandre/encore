//! Integration tests for pylitparser-derive.
#![allow(clippy::disallowed_names)]

use std::option;

use pylitparser::{LitParser, ParseResult};
use pylitparser_derive::LitParser;
use ruff_python_ast as ast;

fn parse(src: &str) -> ast::Expr {
    ruff_python_parser::parse_expression(src)
        .expect("failed to parse expression")
        .into_expr()
}

#[test]
fn test_parse_basic() {
    #[derive(LitParser)]
    struct Foo {
        foo: String,
        bar: std::time::Duration,
    }

    let expr = parse(r#"{ "foo": "hello", "bar": "1h" }"#);
    let foo = Foo::parse_lit(&expr).expect("failed to parse lit");
    assert_eq!(foo.foo, "hello");
    assert_eq!(foo.bar, std::time::Duration::from_secs(3600));
}

#[test]
fn test_parse_str_keys() {
    #[derive(LitParser)]
    struct Foo {
        foo: String,
        bar: i32,
    }

    let expr = parse(r#"{ "foo": "foo", "bar": 3 }"#);
    let foo = Foo::parse_lit(&expr).expect("failed to parse lit");
    assert_eq!(foo.foo, "foo");
    assert_eq!(foo.bar, 3);
}

#[test]
#[allow(dead_code)]
fn test_parse_refs() {
    struct Dummy<'a> {
        foo: Option<&'a str>,
    }
    impl<'py> LitParser<'py> for Dummy<'py> {
        fn parse_lit(_input: &'py ast::Expr) -> ParseResult<Self> {
            Ok(Self { foo: None })
        }
    }

    #[derive(LitParser)]
    struct Foo<'py> {
        foo: String,
        dummy: Dummy<'py>,
    }

    let expr = parse(r#"{ "foo": "foo", "dummy": None }"#);
    let foo = Foo::parse_lit(&expr).expect("failed to parse lit");
    assert_eq!(foo.foo, "foo");
}

#[test]
fn test_parse_option() {
    #[derive(LitParser)]
    struct Foo {
        // Try with different ways of writing Option, since we parse the syntax tree.
        foo: Option<String>,
        bar: option::Option<String>,
        baz: std::option::Option<String>,
        boo: ::std::option::Option<String>,
    }

    // The empty case
    {
        let expr = parse(r#"{ }"#);
        let foo = Foo::parse_lit(&expr).expect("failed to parse lit");
        assert_eq!(foo.foo, None);
        assert_eq!(foo.bar, None);
        assert_eq!(foo.baz, None);
        assert_eq!(foo.boo, None);
    }

    // The non-empty case
    {
        let expr = parse(r#"{ "foo": "foo", "bar": "bar", "baz": "baz", "boo": "boo" }"#);
        let foo = Foo::parse_lit(&expr).expect("failed to parse lit");
        assert_eq!(foo.foo, Some("foo".to_string()));
        assert_eq!(foo.bar, Some("bar".to_string()));
        assert_eq!(foo.baz, Some("baz".to_string()));
        assert_eq!(foo.boo, Some("boo".to_string()));
    }
}

#[test]
fn test_parse_integers() {
    #[derive(LitParser)]
    struct Numbers {
        small: i32,
        big: i64,
        unsigned: u32,
    }

    let expr = parse(r#"{ "small": 42, "big": 9999999999, "unsigned": 100 }"#);
    let nums = Numbers::parse_lit(&expr).expect("failed to parse lit");
    assert_eq!(nums.small, 42);
    assert_eq!(nums.big, 9999999999);
    assert_eq!(nums.unsigned, 100);
}

#[test]
fn test_parse_negative_numbers() {
    #[derive(LitParser)]
    struct Numbers {
        neg: i32,
    }

    let expr = parse(r#"{ "neg": -42 }"#);
    let nums = Numbers::parse_lit(&expr).expect("failed to parse lit");
    assert_eq!(nums.neg, -42);
}

#[test]
fn test_parse_booleans() {
    #[derive(LitParser)]
    struct Flags {
        enabled: bool,
        disabled: bool,
    }

    let expr = parse(r#"{ "enabled": True, "disabled": False }"#);
    let flags = Flags::parse_lit(&expr).expect("failed to parse lit");
    assert!(flags.enabled);
    assert!(!flags.disabled);
}

#[test]
fn test_parse_list() {
    #[derive(LitParser)]
    struct Config {
        items: Vec<String>,
    }

    let expr = parse(r#"{ "items": ["a", "b", "c"] }"#);
    let config = Config::parse_lit(&expr).expect("failed to parse lit");
    assert_eq!(config.items, vec!["a", "b", "c"]);
}

#[test]
fn test_parse_nullable() {
    use pylitparser::Nullable;

    #[derive(LitParser)]
    struct Config {
        value: Nullable<String>,
    }

    // Test with None
    let expr = parse(r#"{ "value": None }"#);
    let config = Config::parse_lit(&expr).expect("failed to parse lit");
    assert!(matches!(config.value, Nullable::Null));

    // Test with value
    let expr = parse(r#"{ "value": "hello" }"#);
    let config = Config::parse_lit(&expr).expect("failed to parse lit");
    match config.value {
        Nullable::Present(s) => assert_eq!(s, "hello"),
        Nullable::Null => panic!("expected Present"),
    }
}

#[test]
fn test_error_duplicate_field() {
    #[derive(Debug, LitParser)]
    struct Foo {
        foo: String,
    }

    let expr = parse(r#"{ "foo": "a", "foo": "b" }"#);
    let result = Foo::parse_lit(&expr);
    assert!(result.is_err());
    let err = result.unwrap_err();
    assert!(err.message.contains("set twice"));
}

#[test]
fn test_error_unknown_field() {
    #[derive(Debug, LitParser)]
    struct Foo {
        foo: String,
    }

    let expr = parse(r#"{ "foo": "a", "unknown": "b" }"#);
    let result = Foo::parse_lit(&expr);
    assert!(result.is_err());
    let err = result.unwrap_err();
    assert!(err.message.contains("unexpected field"));
}

#[test]
fn test_error_missing_required_field() {
    #[derive(Debug, LitParser)]
    struct Foo {
        foo: String,
        bar: String,
    }

    let expr = parse(r#"{ "foo": "a" }"#);
    let result = Foo::parse_lit(&expr);
    assert!(result.is_err());
    let err = result.unwrap_err();
    assert!(err.message.contains("required"));
    assert!(err.message.contains("bar"));
}

#[test]
fn test_error_not_a_dict() {
    #[derive(Debug, LitParser)]
    struct Foo {
        foo: String,
    }

    let expr = parse(r#""not a dict""#);
    let result = Foo::parse_lit(&expr);
    assert!(result.is_err());
    let err = result.unwrap_err();
    assert!(err.message.contains("dictionary literal"));
}

#[test]
fn test_ast_ref_field() {
    #[derive(LitParser)]
    struct Handler<'py> {
        name: String,
        handler: &'py ast::Expr,
    }

    let expr = parse(r#"{ "name": "my_handler", "handler": some_func }"#);
    let handler = Handler::parse_lit(&expr).expect("failed to parse lit");
    assert_eq!(handler.name, "my_handler");
    // The handler field should be a reference to an Expr::Name
    match handler.handler {
        ast::Expr::Name(name) => {
            assert_eq!(name.id.as_str(), "some_func");
        }
        _ => panic!("expected Name expression"),
    }
}

#[test]
fn test_optional_ast_ref_field() {
    #[derive(LitParser)]
    struct Config<'py> {
        name: String,
        handler: Option<&'py ast::Expr>,
    }

    // Test without handler
    {
        let expr = parse(r#"{ "name": "config" }"#);
        let config = Config::parse_lit(&expr).expect("failed to parse lit");
        assert_eq!(config.name, "config");
        assert!(config.handler.is_none());
    }

    // Test with handler
    {
        let expr = parse(r#"{ "name": "config", "handler": my_handler }"#);
        let config = Config::parse_lit(&expr).expect("failed to parse lit");
        assert_eq!(config.name, "config");
        assert!(config.handler.is_some());
        match config.handler.unwrap() {
            ast::Expr::Name(name) => {
                assert_eq!(name.id.as_str(), "my_handler");
            }
            _ => panic!("expected Name expression"),
        }
    }
}
