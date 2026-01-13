//! Secret resource definition and parsing.

use std::sync::Arc;

use ruff_text_size::TextRange;

use crate::ast::loader::errors::ParseResult;
use crate::ast::loader::fileset::Span;
use crate::ast::scoping::references::{iter_references, ReferenceContext, ReferenceParser, TrackedNames};
use crate::parser::resourceparser::{ResourceParseContext, ResourceParser};
use crate::parser::resources::{BindKind, Resource};

/// A secret definition.
#[derive(Debug, Clone)]
pub struct Secret {
    /// The secret name.
    pub name: String,
    /// Source range of the secret definition.
    pub span: Span,
    /// Documentation comment.
    pub doc: Option<String>,
}

impl Secret {
    /// Creates a new Secret.
    pub fn new(name: String, span: Span) -> Self {
        Self {
            name,
            span,
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

/// Intermediate struct for parsed secret definitions.
pub(super) struct SecretDef {
    /// Resource name (first string arg).
    pub(super) resource_name: String,
    /// Variable name being assigned to.
    pub(super) bind_name: Option<String>,
    /// Text range for span conversion.
    pub(super) range: TextRange,
}

impl ReferenceParser<'_> for SecretDef {
    fn parse_resource_reference(ctx: &ReferenceContext<'_, '_>) -> ParseResult<Option<Self>> {
        // First arg must be the secret name (string)
        let Some(resource_name) = ctx.first_string_arg() else {
            return Err(ctx.error("secret() requires a name as the first argument"));
        };

        Ok(Some(SecretDef {
            resource_name,
            bind_name: ctx.bind_name.clone(),
            range: ctx.range(),
        }))
    }
}

fn parse_secrets(ctx: &mut ResourceParseContext) -> ParseResult<()> {
    // Track both "secret" and "Secret" from both modules
    let names = TrackedNames::new(&[
        ("encoredev.config", "secret"),
        ("encoredev.config", "Secret"),
        ("encoredev.config.secrets", "secret"),
        ("encoredev.config.secrets", "Secret"),
    ]);

    let module = &ctx.module_obj.module;
    let results: Vec<ParseResult<SecretDef>> = iter_references(
        &module.ast,
        &names,
        Some(module.mod_path.clone()),
        module.is_package,
        module.file.id,
    );

    for result in results {
        let secret_def = result?;

        let secret = Secret::new(secret_def.resource_name, ctx.span(secret_def.range));

        let resource = Resource::Secret(Arc::new(secret));
        ctx.add_bind(
            resource.clone(),
            BindKind::Create,
            ctx.span(secret_def.range),
            secret_def.bind_name,
        );
        ctx.add_resource(resource);
    }

    Ok(())
}
