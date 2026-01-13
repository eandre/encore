//! Object storage bucket resource definition and parsing.

use std::sync::Arc;

use ruff_python_ast as ast;

use crate::parser::fileset::Range;
use crate::parser::resourceparser::{ResourceParseContext, ResourceParser};
use crate::parser::resources::parseutil::{get_first_string_arg, get_second_arg, is_class_call, KeywordArgs};
use crate::parser::resources::{BindKind, Resource};

/// A bucket (object storage) definition.
#[derive(Debug, Clone)]
pub struct Bucket {
    /// The bucket name.
    pub name: String,
    /// Source range of the bucket definition.
    pub range: Range,
    /// Whether objects are publicly accessible.
    pub public: bool,
    /// Whether versioning is enabled.
    pub versioned: bool,
    /// Documentation comment.
    pub doc: Option<String>,
}

impl Bucket {
    /// Creates a new bucket.
    pub fn new(name: String, range: Range) -> Self {
        Bucket {
            name,
            range,
            public: false,
            versioned: false,
            doc: None,
        }
    }
}

/// Parser for bucket definitions.
pub static BUCKET_PARSER: ResourceParser = ResourceParser {
    name: "bucket",
    interesting_pkgs: &["encoredev.storage.objects"],
    run: parse_buckets,
};

fn parse_buckets(ctx: &mut ResourceParseContext) {
    let assignments: Vec<_> = ctx.module.assignments().to_vec();

    for assignment in assignments {
        let call = match &*assignment.value {
            ast::Expr::Call(c) => c,
            _ => continue,
        };

        if !is_class_call(call, "Bucket")
            || !ctx.is_imported_from("Bucket", "encoredev.storage.objects")
        {
            continue;
        }

        let Some(name) = get_first_string_arg(call) else {
            continue;
        };

        let mut bucket = Bucket::new(name.clone(), ctx.range(assignment.range.to_text_range()));

        // Check for BucketConfig
        if let Some(config_expr) = get_second_arg(call) {
            if let ast::Expr::Call(config_call) = config_expr {
                let kwargs = KeywordArgs::new(&config_call.arguments.keywords);
                if let Some(public) = kwargs.get_bool("public") {
                    bucket.public = public;
                }
                if let Some(versioned) = kwargs.get_bool("versioned") {
                    bucket.versioned = versioned;
                }
            }
        }

        let resource = Resource::Bucket(Arc::new(bucket));
        ctx.add_bind(
            resource.clone(),
            BindKind::Create,
            ctx.range(assignment.range.to_text_range()),
            Some(assignment.name.clone()),
        );
        ctx.add_resource(resource);
    }
}
