//! Pub/Sub subscription resource definition and parsing.

use std::sync::Arc;

use ruff_python_ast as ast;

use crate::parser::fileset::Range;
use crate::parser::resourceparser::{ResourceParseContext, ResourceParser};
use crate::parser::resources::parseutil::{extract_string, is_class_call, KeywordArgs};
use crate::parser::resources::{BindKind, Resource};

/// A Pub/Sub subscription definition.
#[derive(Debug, Clone)]
pub struct Subscription {
    /// The subscription name.
    pub name: String,
    /// Source range of the subscription definition.
    pub range: Range,
    /// The topic this subscription subscribes to.
    pub topic_name: String,
    /// Maximum concurrent handlers.
    pub max_concurrency: Option<i32>,
    /// Acknowledgement deadline.
    pub ack_deadline: Option<String>,
    /// Message retention duration.
    pub message_retention: Option<String>,
    /// The handler function name.
    pub handler_name: Option<String>,
    /// The service this subscription belongs to.
    pub service_name: Option<String>,
    /// Documentation comment.
    pub doc: Option<String>,
}

impl Subscription {
    /// Creates a new subscription.
    pub fn new(name: String, range: Range, topic_name: String) -> Self {
        Subscription {
            name,
            range,
            topic_name,
            max_concurrency: None,
            ack_deadline: None,
            message_retention: None,
            handler_name: None,
            service_name: None,
            doc: None,
        }
    }
}

/// Parser for subscription definitions.
pub static SUBSCRIPTION_PARSER: ResourceParser = ResourceParser {
    name: "subscription",
    interesting_pkgs: &["encoredev.pubsub"],
    run: parse_subscriptions,
};

fn parse_subscriptions(ctx: &mut ResourceParseContext) {
    let assignments: Vec<_> = ctx.module.assignments().to_vec();

    for assignment in assignments {
        let call = match &*assignment.value {
            ast::Expr::Call(c) => c,
            _ => continue,
        };

        if !is_class_call(call, "Subscription")
            || !ctx.is_imported_from("Subscription", "encoredev.pubsub")
        {
            continue;
        }

        // First arg is the topic reference
        let topic_name = match call.arguments.args.first() {
            Some(ast::Expr::Name(n)) => n.id.to_string(),
            _ => continue,
        };

        // Second arg is the subscription name
        let Some(name) = call.arguments.args.get(1).and_then(extract_string) else {
            continue;
        };

        let mut sub = Subscription::new(
            name.clone(),
            ctx.range(assignment.range.to_text_range()),
            topic_name,
        );
        sub.service_name = ctx.service_name.clone();

        // Check for SubscriptionConfig
        if let Some(config_expr) = call.arguments.args.get(2) {
            if let ast::Expr::Call(config_call) = config_expr {
                let kwargs = KeywordArgs::new(&config_call.arguments.keywords);

                if let Some(concurrency) = kwargs.get_int("max_concurrency") {
                    sub.max_concurrency = Some(concurrency as i32);
                }
                if let Some(deadline) = kwargs.get_string("ack_deadline") {
                    sub.ack_deadline = Some(deadline);
                }
                if let Some(retention) = kwargs.get_string("message_retention") {
                    sub.message_retention = Some(retention);
                }

                // Extract handler function name
                if let Some(handler_expr) = kwargs.get("handler") {
                    if let ast::Expr::Name(n) = handler_expr {
                        sub.handler_name = Some(n.id.to_string());
                    }
                }
            }
        }

        let resource = Resource::PubSubSubscription(Arc::new(sub));
        ctx.add_bind(
            resource.clone(),
            BindKind::Create,
            ctx.range(assignment.range.to_text_range()),
            Some(assignment.name.clone()),
        );
        ctx.add_resource(resource);
    }
}
