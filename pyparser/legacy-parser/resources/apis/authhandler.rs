//! Authentication handler resource definition and parsing.

use std::sync::Arc;

use ruff_python_ast as ast;

use crate::parser::fileset::Range;
use crate::parser::resourceparser::{ResourceParseContext, ResourceParser};
use crate::parser::resources::parseutil::is_decorator_call;
use crate::parser::resources::{BindKind, Resource};

/// An authentication handler definition.
#[derive(Debug, Clone)]
pub struct AuthHandler {
    /// The handler name (function name).
    pub name: String,
    /// Source range of the handler definition.
    pub range: Range,
    /// Documentation comment.
    pub doc: Option<String>,
    /// The service this handler belongs to.
    pub service_name: Option<String>,
}

impl AuthHandler {
    /// Creates a new auth handler.
    pub fn new(name: String, range: Range) -> Self {
        AuthHandler {
            name,
            range,
            doc: None,
            service_name: None,
        }
    }
}

/// Parser for auth handler definitions.
pub static AUTHHANDLER_PARSER: ResourceParser = ResourceParser {
    name: "authhandler",
    interesting_pkgs: &["encoredev.auth"],
    run: parse_auth_handlers,
};

fn parse_auth_handlers(ctx: &mut ResourceParseContext) {
    // Collect function defs first
    let func_defs: Vec<_> = ctx
        .module
        .ast
        .body
        .iter()
        .filter_map(|stmt| {
            if let ast::Stmt::FunctionDef(func) = stmt {
                Some(func.clone())
            } else {
                None
            }
        })
        .collect();

    for func in func_defs {
        for decorator in &func.decorator_list {
            if is_decorator_call(decorator, "auth_handler")
                && ctx.is_imported_from("auth_handler", "encoredev.auth")
            {
                let handler = AuthHandler {
                    name: func.name.to_string(),
                    range: ctx.range(func.range),
                    doc: None,
                    service_name: ctx.service_name.clone(),
                };

                let resource = Resource::AuthHandler(Arc::new(handler));
                ctx.add_bind(
                    resource.clone(),
                    BindKind::Create,
                    ctx.range(func.range),
                    Some(func.name.to_string()),
                );
                ctx.add_resource(resource);
                break;
            }
        }
    }
}
