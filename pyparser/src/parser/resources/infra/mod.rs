//! Infrastructure resource types and parsers.
//!
//! This module contains resources related to infrastructure:
//! - Cron jobs
//! - Metrics
//! - Object storage (buckets)
//! - Pub/Sub (topics and subscriptions)
//! - Secrets
//! - SQL databases

#[cfg(test)]
mod tests;

pub mod cron;
pub mod metrics;
pub mod objects;
pub mod pubsub_subscription;
pub mod pubsub_topic;
pub mod secret;
pub mod sqldb;

// Re-export types
pub use cron::CronJob;
pub use metrics::{Metric, MetricType};
pub use objects::Bucket;
pub use pubsub_subscription::Subscription;
pub use pubsub_topic::{DeliveryGuarantee, Topic};
pub use secret::Secret;
pub use sqldb::{MigrationSource, SQLDatabase};

// Re-export parsers
pub use cron::CRON_PARSER;
pub use metrics::METRIC_PARSER;
pub use objects::BUCKET_PARSER;
pub use pubsub_subscription::SUBSCRIPTION_PARSER;
pub use pubsub_topic::TOPIC_PARSER;
pub use secret::SECRET_PARSER;
pub use sqldb::SQLDB_PARSER;
