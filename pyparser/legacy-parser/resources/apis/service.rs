//! Service resource definition and parsing.

use std::sync::Arc;

use ruff_python_ast as ast;

use crate::parser::fileset::Range;
use crate::parser::resourceparser::{ResourceParseContext, ResourceParser};
use crate::parser::resources::parseutil::{get_first_string_arg, is_class_call};
use crate::parser::resources::{BindKind, Resource};

/// A service definition.
#[derive(Debug, Clone)]
pub struct Service {
    /// The service name.
    pub name: String,
    /// Source range of the service definition.
    pub range: Range,
    /// The relative path of the service directory.
    pub rel_path: String,
    /// Documentation comment.
    pub doc: Option<String>,
}

/// Parser for service definitions.
pub static SERVICE_PARSER: ResourceParser = ResourceParser {
    name: "service",
    interesting_pkgs: &["encoredev.service"],
    run: parse_services,
};

fn parse_services(ctx: &mut ResourceParseContext) {
    // Collect assignments first to avoid borrow issues
    let assignments: Vec<_> = ctx.module.assignments().to_vec();

    for assignment in assignments {
        let call = match &*assignment.value {
            ast::Expr::Call(c) => c,
            _ => continue,
        };

        if !is_class_call(call, "Service") || !ctx.is_imported_from("Service", "encoredev.service")
        {
            continue;
        }

        // Extract the service name from the first argument
        let Some(name) = get_first_string_arg(call) else {
            continue;
        };

        // Get the relative path from the file path
        let rel_path = ctx
            .module
            .file_path
            .as_real()
            .and_then(|p| p.parent())
            .map(|p| p.to_string_lossy().to_string())
            .unwrap_or_default();

        let service = Service {
            name: name.clone(),
            range: ctx.range(assignment.range.to_text_range()),
            rel_path,
            doc: None,
        };

        let resource = Resource::Service(Arc::new(service));
        ctx.service_name = Some(name.clone());
        ctx.add_bind(
            resource.clone(),
            BindKind::Create,
            ctx.range(assignment.range.to_text_range()),
            Some(assignment.name.clone()),
        );
        ctx.add_resource(resource);
    }
}
