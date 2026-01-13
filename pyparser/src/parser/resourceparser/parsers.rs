//! Resource parser implementations.
//!
//! This module re-exports all resource parsers from their respective modules.

use super::ResourceParser;
use crate::parser::resources::{
    AUTHHANDLER_PARSER, BUCKET_PARSER, CRON_PARSER, ENDPOINT_PARSER, GATEWAY_PARSER, METRIC_PARSER,
    SECRET_PARSER, SERVICE_PARSER, SQLDB_PARSER, SUBSCRIPTION_PARSER, TOPIC_PARSER,
};

/// List of all default resource parsers.
pub static DEFAULT_RESOURCE_PARSERS: &[&ResourceParser] = &[
    &SERVICE_PARSER,
    &ENDPOINT_PARSER,
    &AUTHHANDLER_PARSER,
    &GATEWAY_PARSER,
    &SQLDB_PARSER,
    &BUCKET_PARSER,
    &TOPIC_PARSER,
    &SUBSCRIPTION_PARSER,
    &CRON_PARSER,
    &SECRET_PARSER,
    &METRIC_PARSER,
];
