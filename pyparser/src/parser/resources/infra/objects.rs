//! Object storage bucket resource definition and parsing.

use std::sync::Arc;

use pylitparser::LitParser;
use pylitparser_derive::LitParser;
use ruff_text_size::{Ranged, TextRange};

use crate::ast::loader::errors::ParseResult;
use crate::ast::loader::fileset::Span;
use crate::ast::scoping::references::{iter_references, ReferenceContext, ReferenceParser, TrackedNames};
use crate::parser::resourceparser::{ResourceParseContext, ResourceParser};
use crate::parser::resources::{BindKind, Resource};

/// A bucket (object storage) definition.
#[derive(Debug, Clone)]
pub struct Bucket {
    /// The bucket name.
    pub name: String,
    /// Source range of the bucket definition.
    pub span: Span,
    /// Whether objects are publicly accessible.
    pub public: bool,
    /// Whether versioning is enabled.
    pub versioned: bool,
    /// Documentation comment.
    pub doc: Option<String>,
}

impl Bucket {
    /// Creates a new Bucket.
    pub fn new(name: String, span: Span) -> Self {
        Self {
            name,
            span,
            public: false,
            versioned: false,
            doc: None,
        }
    }
}

/// Configuration for a bucket, parsed from Python dict literal.
#[derive(Debug, Default, LitParser)]
struct DecodedBucketConfig {
    /// Whether objects are publicly accessible.
    public: Option<bool>,
    /// Whether versioning is enabled.
    versioned: Option<bool>,
}

/// Parser for bucket definitions.
pub static BUCKET_PARSER: ResourceParser = ResourceParser {
    name: "bucket",
    interesting_pkgs: &["encoredev.storage.objects"],
    run: parse_buckets,
};

/// Intermediate struct for parsed bucket definitions.
pub(super) struct BucketDef {
    /// Resource name (first string arg).
    pub(super) resource_name: String,
    /// Whether objects are publicly accessible.
    pub(super) public: bool,
    /// Whether versioning is enabled.
    pub(super) versioned: bool,
    /// Variable name being assigned to.
    pub(super) bind_name: Option<String>,
    /// Text range for span conversion.
    pub(super) range: TextRange,
}

impl ReferenceParser<'_> for BucketDef {
    fn parse_resource_reference(ctx: &ReferenceContext<'_, '_>) -> ParseResult<Option<Self>> {
        // First arg must be the bucket name (string)
        let Some(resource_name) = ctx.first_string_arg() else {
            return Err(ctx.error("Bucket() requires a name as the first argument"));
        };

        // Second arg must be the config dict
        let Some(config_expr) = ctx.arg(1) else {
            return Err(ctx.error("Bucket() requires a config as the second argument"));
        };

        let config = DecodedBucketConfig::parse_lit(config_expr)
            .map_err(|e| ctx.error_at(config_expr.range(), format!("invalid bucket config: {}", e)))?;

        Ok(Some(BucketDef {
            resource_name,
            public: config.public.unwrap_or(false),
            versioned: config.versioned.unwrap_or(false),
            bind_name: ctx.bind_name.clone(),
            range: ctx.range(),
        }))
    }
}

fn parse_buckets(ctx: &mut ResourceParseContext) -> ParseResult<()> {
    let names = TrackedNames::new(&[("encoredev.storage.objects", "Bucket")]);

    let module = &ctx.module_obj.module;
    let results: Vec<ParseResult<BucketDef>> = iter_references(
        &module.ast,
        &names,
        Some(module.mod_path.clone()),
        module.is_package,
        module.file.id,
    );

    for result in results {
        let bucket_def = result?;

        let mut bucket = Bucket::new(bucket_def.resource_name, ctx.span(bucket_def.range));
        bucket.public = bucket_def.public;
        bucket.versioned = bucket_def.versioned;

        let resource = Resource::Bucket(Arc::new(bucket));
        ctx.add_bind(
            resource.clone(),
            BindKind::Create,
            ctx.span(bucket_def.range),
            bucket_def.bind_name,
        );
        ctx.add_resource(resource);
    }

    Ok(())
}
