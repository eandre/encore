//! Pub/Sub topic resource definition and parsing.

use std::sync::Arc;

use ruff_python_ast as ast;

use crate::parser::fileset::Range;
use crate::parser::resourceparser::{ResourceParseContext, ResourceParser};
use crate::parser::resources::parseutil::{get_first_string_arg, get_second_arg, is_class_call};
use crate::parser::resources::{BindKind, Resource};

/// Delivery guarantee for pub/sub.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DeliveryGuarantee {
    /// At least once delivery.
    AtLeastOnce,
    /// Exactly once delivery (best effort).
    ExactlyOnce,
}

/// A Pub/Sub topic definition.
#[derive(Debug, Clone)]
pub struct Topic {
    /// The topic name.
    pub name: String,
    /// Source range of the topic definition.
    pub range: Range,
    /// Delivery guarantee.
    pub delivery_guarantee: DeliveryGuarantee,
    /// Ordering attribute (if any).
    pub ordering_attribute: Option<String>,
    /// Documentation comment.
    pub doc: Option<String>,
}

impl Topic {
    /// Creates a new topic.
    pub fn new(name: String, range: Range, delivery_guarantee: DeliveryGuarantee) -> Self {
        Topic {
            name,
            range,
            delivery_guarantee,
            ordering_attribute: None,
            doc: None,
        }
    }
}

/// Parser for topic definitions.
pub static TOPIC_PARSER: ResourceParser = ResourceParser {
    name: "topic",
    interesting_pkgs: &["encoredev.pubsub"],
    run: parse_topics,
};

fn parse_topics(ctx: &mut ResourceParseContext) {
    let assignments: Vec<_> = ctx.module.assignments().to_vec();

    for assignment in assignments {
        let call = match &*assignment.value {
            ast::Expr::Call(c) => c,
            _ => continue,
        };

        if !is_class_call(call, "Topic") || !ctx.is_imported_from("Topic", "encoredev.pubsub") {
            continue;
        }

        let Some(name) = get_first_string_arg(call) else {
            continue;
        };

        let mut delivery = DeliveryGuarantee::AtLeastOnce;
        let mut ordering_attr = None;

        // Check for TopicConfig (dict literal)
        if let Some(config_expr) = get_second_arg(call) {
            if let ast::Expr::Dict(dict) = config_expr {
                for (key, value) in dict.iter_keys().zip(dict.iter_values()) {
                    let Some(key_expr) = key else { continue };
                    let ast::Expr::StringLiteral(key_str) = key_expr else { continue };
                    let key_name = key_str.value.to_str();

                    match key_name {
                        "delivery_guarantee" => {
                            if let ast::Expr::StringLiteral(val) = value {
                                delivery = match val.value.to_str() {
                                    "exactly-once" => DeliveryGuarantee::ExactlyOnce,
                                    _ => DeliveryGuarantee::AtLeastOnce,
                                };
                            }
                        }
                        "ordering_attribute" => {
                            if let ast::Expr::StringLiteral(val) = value {
                                ordering_attr = Some(val.value.to_str().to_string());
                            }
                        }
                        _ => {}
                    }
                }
            }
        }

        let mut topic = Topic::new(
            name.clone(),
            ctx.range(assignment.range.to_text_range()),
            delivery,
        );
        topic.ordering_attribute = ordering_attr;

        let resource = Resource::PubSubTopic(Arc::new(topic));
        ctx.add_bind(
            resource.clone(),
            BindKind::Create,
            ctx.range(assignment.range.to_text_range()),
            Some(assignment.name.clone()),
        );
        ctx.add_resource(resource);
    }
}
