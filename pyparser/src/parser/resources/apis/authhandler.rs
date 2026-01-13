//! Authentication handler resource definition and parsing.

use std::sync::Arc;

use ruff_python_ast as ast;

use crate::ast::loader::errors::ParseResult;
use crate::ast::loader::fileset::Span;
use crate::parser::resourceparser::{ResourceParseContext, ResourceParser};
use crate::parser::resources::parseutil::is_decorator_call;
use crate::parser::resources::{BindKind, Resource};

/// An authentication handler definition.
#[derive(Debug, Clone)]
pub struct AuthHandler {
    /// The handler name (function name).
    pub name: String,
    /// Source range of the handler definition.
    pub span: Span,
    /// Documentation comment.
    pub doc: Option<String>,
    /// The service this handler belongs to.
    pub service_name: Option<String>,
}

/// Parser for auth handler definitions.
pub static AUTHHANDLER_PARSER: ResourceParser = ResourceParser {
    name: "authhandler",
    interesting_pkgs: &["encoredev.auth"],
    run: parse_auth_handlers,
};

fn parse_auth_handlers(ctx: &mut ResourceParseContext) -> ParseResult<()> {
    // Collect function defs first
    let func_defs: Vec<_> = ctx
        .module_obj
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
            if is_decorator_call(decorator, "auth_handler") {
                let func_span = ctx.span(func.range);
                let handler = AuthHandler {
                    name: func.name.to_string(),
                    span: func_span,
                    doc: None,
                    service_name: ctx.service_name.clone(),
                };

                let resource = Resource::AuthHandler(Arc::new(handler));
                ctx.add_bind(
                    resource.clone(),
                    BindKind::Create,
                    func_span,
                    Some(func.name.to_string()),
                );
                ctx.add_resource(resource);
                break;
            }
        }
    }
    Ok(())
}
