//! Pub/Sub subscription resource definition and parsing.

use std::sync::Arc;

use pylitparser::LitParser;
use pylitparser_derive::LitParser;
use ruff_python_ast as ast;
use ruff_text_size::{Ranged, TextRange};

use crate::ast::loader::errors::{ErrorReporter, ParseResult};
use crate::ast::loader::fileset::Span;
use crate::ast::schema::object::Object;
use crate::ast::scoping::references::{
    iter_references, ReferenceContext, ReferenceParser, TrackedNames,
};
use crate::parser::resourceparser::{ResourceParseContext, ResourceParser};
use crate::parser::resources::{BindKind, Resource};

/// A Pub/Sub subscription definition.
#[derive(Debug, Clone)]
pub struct Subscription<'py> {
    /// The subscription name.
    pub name: String,
    /// Source range of the subscription definition.
    pub span: Span,
    /// The topic this subscription subscribes to.
    pub topic: &'py dyn Object,
    /// The handler to execute.
    pub handler: &'py ast::Expr,
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

impl<'py> Subscription<'py> {
    /// Creates a new Subscription.
    pub fn new(name: String, span: Span, topic: &'py dyn Object, handler: &'py ast::Expr) -> Self {
        Self {
            name,
            span,
            topic,
            handler,
            max_concurrency: None,
            ack_deadline: None,
            message_retention: None,
            handler_name: None,
            service_name: None,
            doc: None,
        }
    }
}

/// Configuration for a subscription, parsed from Python dict literal.
#[derive(Debug, LitParser)]
struct DecodedSubscriptionConfig<'py> {
    handler: &'py ast::Expr,

    /// Maximum concurrent handlers.
    max_concurrency: Option<i64>,
    /// Acknowledgement deadline.
    ack_deadline: Option<String>,
    /// Message retention duration.
    message_retention: Option<String>,
}

/// Parser for subscription definitions.
pub static SUBSCRIPTION_PARSER: ResourceParser = ResourceParser {
    name: "subscription",
    interesting_pkgs: &["encoredev.pubsub"],
    run: parse_subscriptions,
};

/// Intermediate struct for parsed subscription definitions.
struct SubscriptionDef<'py> {
    /// Subscription name (second string arg).
    subscription_name: String,
    /// Topic reference name.
    topic_expr: &'py ast::Expr,
    /// The subscription handler.
    handler: &'py ast::Expr,
    /// Maximum concurrent handlers.
    max_concurrency: Option<i32>,
    /// Acknowledgement deadline.
    ack_deadline: Option<String>,
    /// Message retention duration.
    message_retention: Option<String>,
    /// Variable name being assigned to.
    bind_name: Option<String>,
    /// Text range for span conversion.
    range: TextRange,
}

impl<'py> ReferenceParser<'py> for SubscriptionDef<'py> {
    fn parse_resource_reference(ctx: &ReferenceContext<'_, 'py>) -> ParseResult<Option<Self>> {
        // First arg is the topic reference (a name)
        let Some(topic_expr) = ctx.arg(0) else {
            return Err(ctx.error("Subscription() requires a topic as the first argument"));
        };

        // Second arg is the subscription name (string)
        let Some(second_arg) = ctx.arg(1) else {
            return Err(ctx.error("Subscription() requires a name as the second argument"));
        };
        let subscription_name = match second_arg {
            ast::Expr::StringLiteral(s) => s.value.to_string(),
            _ => {
                return Err(ctx.error_at(
                    second_arg.range(),
                    "Subscription() second argument must be a string",
                ))
            }
        };

        // Third arg must be the config dict
        let Some(config_expr) = ctx.arg(2) else {
            return Err(ctx.error("Subscription() requires a config as the third argument"));
        };

        let config = DecodedSubscriptionConfig::parse_lit(config_expr).map_err(|e| {
            ctx.error_at(
                config_expr.range(),
                format!("invalid subscription config: {}", e),
            )
        })?;

        Ok(Some(SubscriptionDef {
            subscription_name,
            topic_expr,
            handler: config.handler,
            max_concurrency: config.max_concurrency.map(|v| v as i32),
            ack_deadline: config.ack_deadline,
            message_retention: config.message_retention,
            bind_name: ctx.bind_name.clone(),
            range: ctx.range(),
        }))
    }
}

fn parse_subscriptions(ctx: &mut ResourceParseContext) -> ParseResult<()> {
    let names = TrackedNames::new(&[("encoredev.pubsub", "Subscription")]);

    let module = &ctx.module_obj.module;
    let results: Vec<ParseResult<SubscriptionDef<'_>>> = iter_references(
        &module.ast,
        &names,
        Some(module.mod_path.clone()),
        module.is_package,
        module.file.id,
    );

    for result in results {
        let sub_def = result?;

        let topic = ctx
            .resolver
            .resolve_obj(ctx.module_obj, sub_def.topic_expr)?
            .ok_or(ctx.span(sub_def.topic_expr).err("unknown topic"))?;

        let mut sub = Subscription::new(
            sub_def.subscription_name,
            ctx.span(sub_def.range),
            topic,
            sub_def.handler,
        );
        sub.max_concurrency = sub_def.max_concurrency;
        sub.ack_deadline = sub_def.ack_deadline;
        sub.message_retention = sub_def.message_retention;
        sub.service_name = ctx.service_name.clone();

        let resource = Resource::PubSubSubscription(Arc::new(sub));
        ctx.add_bind(
            resource.clone(),
            BindKind::Create,
            ctx.span(sub_def.range),
            sub_def.bind_name,
        );
        ctx.add_resource(resource);
    }

    Ok(())
}
