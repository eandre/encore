//! Secret resource definition and parsing.

use std::sync::Arc;

use ruff_python_ast as ast;

use crate::parser::fileset::Range;
use crate::parser::resourceparser::{ResourceParseContext, ResourceParser};
use crate::parser::resources::parseutil::get_first_string_arg;
use crate::parser::resources::{BindKind, Resource};

/// A secret definition.
#[derive(Debug, Clone)]
pub struct Secret {
    /// The secret name.
    pub name: String,
    /// Source range of the secret definition.
    pub range: Range,
    /// Documentation comment.
    pub doc: Option<String>,
}

impl Secret {
    /// Creates a new secret.
    pub fn new(name: String, range: Range) -> Self {
        Secret {
            name,
            range,
            doc: None,
        }
    }
}

/// Parser for secret definitions.
pub static SECRET_PARSER: ResourceParser = ResourceParser {
    name: "secret",
    interesting_pkgs: &["encoredev.config", "encoredev.config.secrets"],
    run: parse_secrets,
};

fn parse_secrets(ctx: &mut ResourceParseContext) {
    let assignments: Vec<_> = ctx.module.assignments().to_vec();

    for assignment in assignments {
        let call = match &*assignment.value {
            ast::Expr::Call(c) => c,
            _ => continue,
        };

        let is_secret_call = match &*call.func {
            ast::Expr::Name(n) => {
                (n.id.as_str() == "secret"
                    && (ctx.is_imported_from("secret", "encoredev.config")
                        || ctx.is_imported_from("secret", "encoredev.config.secrets")))
                    || (n.id.as_str() == "Secret"
                        && (ctx.is_imported_from("Secret", "encoredev.config")
                            || ctx.is_imported_from("Secret", "encoredev.config.secrets")))
            }
            _ => false,
        };

        if !is_secret_call {
            continue;
        }

        let Some(name) = get_first_string_arg(call) else {
            continue;
        };

        let secret = Secret::new(name.clone(), ctx.range(assignment.range.to_text_range()));

        let resource = Resource::Secret(Arc::new(secret));
        ctx.add_bind(
            resource.clone(),
            BindKind::Create,
            ctx.range(assignment.range.to_text_range()),
            Some(assignment.name.clone()),
        );
        ctx.add_resource(resource);
    }
}
