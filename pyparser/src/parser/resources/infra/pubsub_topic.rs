//! Pub/Sub topic resource definition and parsing.

use std::sync::Arc;

use pylitparser::LitParser;
use pylitparser_derive::LitParser;
use ruff_python_ast as ast;
use ruff_text_size::{Ranged, TextRange};

use crate::ast::loader::errors::ParseResult;
use crate::ast::loader::fileset::Span;
use crate::ast::schema::Type;
use crate::ast::scoping::references::{
    iter_references, ReferenceContext, ReferenceParser, TrackedNames,
};
use crate::parser::resourceparser::{ResourceParseContext, ResourceParser};
use crate::parser::resources::{BindKind, Resource};

/// Delivery guarantee for pub/sub.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum DeliveryGuarantee {
    /// At least once delivery.
    #[default]
    AtLeastOnce,
    /// Exactly once delivery (best effort).
    ExactlyOnce,
}

/// Configuration for a pub/sub topic, parsed from Python dict literal.
#[derive(Debug, Default, LitParser)]
struct DecodedTopicConfig {
    /// Delivery guarantee.
    delivery_guarantee: Option<String>,
    /// Ordering attribute (if any).
    ordering_attribute: Option<String>,
}

/// A Pub/Sub topic definition.
#[derive(Debug, Clone)]
pub struct Topic<'py> {
    /// The topic name.
    pub name: String,
    /// Source range of the topic definition.
    pub span: Span,
    /// Delivery guarantee.
    pub delivery_guarantee: DeliveryGuarantee,
    /// Ordering attribute (if any).
    pub ordering_attribute: Option<String>,
    /// Documentation comment.
    pub doc: Option<String>,
    /// The message type for this topic.
    pub message_type: Type<'py>,
}

impl<'py> Topic<'py> {
    /// Creates a new Topic.
    pub fn new(
        name: String,
        span: Span,
        delivery_guarantee: DeliveryGuarantee,
        message_type: Type<'py>,
    ) -> Self {
        Self {
            name,
            span,
            delivery_guarantee,
            ordering_attribute: None,
            doc: None,
            message_type,
        }
    }
}

/// Parser for topic definitions.
pub static TOPIC_PARSER: ResourceParser = ResourceParser {
    name: "topic",
    interesting_pkgs: &["encoredev.pubsub"],
    run: parse_topics,
};

/// Intermediate struct for parsed topic definitions.
pub(crate) struct TopicDef<'ast> {
    /// Resource name (first string arg).
    pub resource_name: String,
    /// Delivery guarantee from config.
    pub delivery_guarantee: DeliveryGuarantee,
    /// Ordering attribute from config.
    pub ordering_attribute: Option<String>,
    /// Variable name being assigned to.
    pub bind_name: Option<String>,
    /// Text range for span conversion.
    pub range: TextRange,
    /// The message type expression (from the generic argument).
    pub message_type_expr: &'ast ast::Expr,
}

impl<'ast> ReferenceParser<'ast> for TopicDef<'ast> {
    fn parse_resource_reference(ctx: &ReferenceContext<'_, 'ast>) -> ParseResult<Option<Self>> {
        // Note: By the time we reach here, we already know this is a call to something
        // originally named "Topic" from "encoredev.pubsub" (the filtering happens in
        // check_tracked_reference using the original_name). We don't need to check
        // callee_name() here, which would fail for aliased imports like:
        // `from encoredev.pubsub import Topic as PubSubTopic`

        // Must be a call expression
        let Some(call) = ctx.as_call() else {
            return Ok(None);
        };

        // Extract the message type from the generic argument: Topic[MessageType](...)
        let message_type_expr = match &*call.func {
            ast::Expr::Subscript(sub) => &*sub.slice,
            _ => {
                return Err(ctx.error("Topic requires a message type as a generic argument, e.g. Topic[MyMessage](...)"));
            }
        };

        // First arg must be the topic name (string)
        let Some(resource_name) = ctx.first_string_arg() else {
            return Err(ctx.error("Topic() requires a name as the first argument"));
        };

        // Second arg must be the config dict
        let Some(config_expr) = ctx.arg(1) else {
            return Err(ctx.error("Topic() requires a config as the second argument"));
        };

        let config = DecodedTopicConfig::parse_lit(config_expr).map_err(|e| {
            ctx.error_at(config_expr.range(), format!("invalid topic config: {}", e))
        })?;

        let delivery_guarantee = match config.delivery_guarantee.as_deref() {
            Some("exactly-once") => DeliveryGuarantee::ExactlyOnce,
            _ => DeliveryGuarantee::AtLeastOnce,
        };

        Ok(Some(TopicDef {
            resource_name,
            delivery_guarantee,
            ordering_attribute: config.ordering_attribute,
            bind_name: ctx.bind_name.clone(),
            range: ctx.range(),
            message_type_expr,
        }))
    }
}

fn parse_topics<'py>(ctx: &mut ResourceParseContext<'py>) -> ParseResult<()> {
    let names = TrackedNames::new(&[("encoredev.pubsub", "Topic")]);

    let module = ctx.module_obj.module;
    let results: Vec<ParseResult<TopicDef>> = iter_references(
        &module.ast,
        &names,
        Some(module.mod_path.clone()),
        module.is_package,
        module.file.id,
    );

    for result in results {
        let topic_def = result?;

        // Parse the message type from the generic argument
        let message_type = ctx
            .resolver
            .parse_type(ctx.module_obj, topic_def.message_type_expr)?;

        let mut topic = Topic::new(
            topic_def.resource_name,
            ctx.span(topic_def.range),
            topic_def.delivery_guarantee,
            message_type,
        );
        topic.ordering_attribute = topic_def.ordering_attribute;

        let resource = Resource::PubSubTopic(Arc::new(topic));
        ctx.add_bind(
            resource.clone(),
            BindKind::Create,
            ctx.span(topic_def.range),
            topic_def.bind_name,
        );
        ctx.add_resource(resource);
    }

    Ok(())
}
