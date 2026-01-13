//! API Gateway resource definition and parsing.

use std::sync::Arc;

use ruff_python_ast as ast;

use crate::parser::fileset::Range;
use crate::parser::resourceparser::{ResourceParseContext, ResourceParser};
use crate::parser::resources::parseutil::{is_class_call, KeywordArgs};
use crate::parser::resources::{BindKind, Resource};

/// An API Gateway definition.
#[derive(Debug, Clone)]
pub struct Gateway {
    /// The gateway name.
    pub name: String,
    /// Source range of the gateway definition.
    pub range: Range,
    /// Whether an auth handler is configured.
    pub has_auth_handler: bool,
    /// The service this gateway belongs to.
    pub service_name: Option<String>,
    /// Documentation comment.
    pub doc: Option<String>,
}

impl Gateway {
    /// Creates a new gateway.
    pub fn new(name: String, range: Range) -> Self {
        Gateway {
            name,
            range,
            has_auth_handler: false,
            service_name: None,
            doc: None,
        }
    }
}

/// Parser for gateway definitions.
pub static GATEWAY_PARSER: ResourceParser = ResourceParser {
    name: "gateway",
    interesting_pkgs: &["encoredev.api"],
    run: parse_gateways,
};

fn parse_gateways(ctx: &mut ResourceParseContext) {
    let assignments: Vec<_> = ctx.module.assignments().to_vec();

    for assignment in assignments {
        let call = match &*assignment.value {
            ast::Expr::Call(c) => c,
            _ => continue,
        };

        // Check for Gateway import from encoredev.api or encoredev.api.gateway
        let is_gateway = is_class_call(call, "Gateway")
            && (ctx.is_imported_from("Gateway", "encoredev.api")
                || ctx.is_imported_from("Gateway", "encoredev.api.gateway"));

        if !is_gateway {
            continue;
        }

        let mut gateway = Gateway::new(
            "api-gateway".to_string(),
            ctx.range(assignment.range.to_text_range()),
        );
        gateway.service_name = ctx.service_name.clone();

        // Check for GatewayConfig with auth_handler
        if let Some(ast::Expr::Call(config_call)) = call.arguments.args.first() {
            let kwargs = KeywordArgs::new(&config_call.arguments.keywords);
            if kwargs.get("auth_handler").is_some() {
                gateway.has_auth_handler = true;
            }
        }

        let resource = Resource::Gateway(Arc::new(gateway));
        ctx.add_bind(
            resource.clone(),
            BindKind::Create,
            ctx.range(assignment.range.to_text_range()),
            Some(assignment.name.clone()),
        );
        ctx.add_resource(resource);
    }
}
