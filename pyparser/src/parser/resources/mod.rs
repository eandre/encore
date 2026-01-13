//! Resource types and definitions.
//!
//! This module defines the various resource types that can be discovered
//! in an Encore Python application.

pub mod apis;
pub mod infra;
pub mod parseutil;

use std::sync::Arc;

// Re-export resource types from apis
pub use apis::{
    Access, AuthHandler, Endpoint, EndpointType, Gateway, Method, Service, ServiceClient,
};

// Re-export resource types from infra
pub use infra::{
    Bucket, CronJob, DeliveryGuarantee, Metric, MetricType, MigrationSource, SQLDatabase, Secret,
    Subscription, Topic,
};

// Re-export parsers from apis
pub use apis::{AUTHHANDLER_PARSER, ENDPOINT_PARSER, GATEWAY_PARSER, SERVICE_PARSER};

// Re-export parsers from infra
pub use infra::{
    BUCKET_PARSER, CRON_PARSER, METRIC_PARSER, SECRET_PARSER, SQLDB_PARSER, SUBSCRIPTION_PARSER,
    TOPIC_PARSER,
};

use crate::ast::loader::{
    fileset::{Span, Spanned},
    ModuleId,
};

/// A resource discovered in the application.
#[derive(Debug, Clone)]
pub enum Resource<'py> {
    /// An API endpoint.
    APIEndpoint(Arc<Endpoint<'py>>),
    /// An authentication handler.
    AuthHandler(Arc<AuthHandler>),
    /// A cron job.
    CronJob(Arc<CronJob>),
    /// An API gateway.
    Gateway(Arc<Gateway>),
    /// A metric (counter or gauge).
    Metric(Arc<Metric>),
    /// An object storage bucket.
    Bucket(Arc<Bucket>),
    /// A pub/sub topic.
    PubSubTopic(Arc<Topic<'py>>),
    /// A pub/sub subscription.
    PubSubSubscription(Arc<Subscription<'py>>),
    /// A secret.
    Secret(Arc<Secret>),
    /// A service definition.
    Service(Arc<Service>),
    /// A SQL database.
    SQLDatabase(Arc<SQLDatabase>),
}

impl Resource<'_> {
    /// Returns the resource name.
    pub fn name(&self) -> &str {
        match self {
            Resource::APIEndpoint(e) => &e.name,
            Resource::AuthHandler(h) => &h.name,
            Resource::CronJob(c) => &c.name,
            Resource::Gateway(g) => &g.name,
            Resource::Metric(m) => &m.name,
            Resource::Bucket(b) => &b.name,
            Resource::PubSubTopic(t) => &t.name,
            Resource::PubSubSubscription(s) => &s.name,
            Resource::Secret(s) => &s.name,
            Resource::Service(s) => &s.name,
            Resource::SQLDatabase(d) => &d.name,
        }
    }

    /// Returns a display name for the resource type.
    pub fn type_name(&self) -> &'static str {
        match self {
            Resource::APIEndpoint(_) => "API Endpoint",
            Resource::AuthHandler(_) => "Auth Handler",
            Resource::CronJob(_) => "Cron Job",
            Resource::Gateway(_) => "Gateway",
            Resource::Metric(_) => "Metric",
            Resource::Bucket(_) => "Bucket",
            Resource::PubSubTopic(_) => "Pub/Sub Topic",
            Resource::PubSubSubscription(_) => "Pub/Sub Subscription",
            Resource::Secret(_) => "Secret",
            Resource::Service(_) => "Service",
            Resource::SQLDatabase(_) => "SQL Database",
        }
    }
}

impl Spanned for Resource<'_> {
    fn span(&self) -> Span {
        match self {
            Resource::APIEndpoint(e) => e.span,
            Resource::AuthHandler(h) => h.span,
            Resource::CronJob(c) => c.span,
            Resource::Gateway(g) => g.span,
            Resource::Metric(m) => m.span,
            Resource::Bucket(b) => b.span,
            Resource::PubSubTopic(t) => t.span,
            Resource::PubSubSubscription(s) => s.span,
            Resource::Secret(s) => s.span,
            Resource::Service(s) => s.span,
            Resource::SQLDatabase(d) => d.span,
        }
    }
}

/// The kind of resource binding.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BindKind {
    /// This binding creates a new resource.
    Create,
    /// This binding references an existing resource.
    Reference,
}

/// A unique identifier for a bind.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct BindId(pub u32);

/// A binding that connects a variable to a resource.
#[derive(Debug, Clone)]
pub struct Bind<'py> {
    /// Unique bind ID.
    pub id: BindId,
    /// The source range of the binding.
    pub span: Span,
    /// The bound resource.
    pub resource: Resource<'py>,
    /// Whether this creates or references the resource.
    pub kind: BindKind,
    /// The module where this bind was found.
    pub module_id: ModuleId,
    /// The variable name (if any).
    pub name: Option<String>,
}

impl<'py> Bind<'py> {
    /// Creates a new bind.
    pub fn new(
        id: BindId,
        span: Span,
        resource: Resource<'py>,
        kind: BindKind,
        module_id: ModuleId,
        name: Option<String>,
    ) -> Self {
        Bind {
            id,
            span,
            resource,
            kind,
            module_id,
            name,
        }
    }
}

/// Represents either a resolved resource or a path reference to one.
#[derive(Debug, Clone)]
pub enum ResourceOrPath<'py> {
    /// A resolved resource.
    Resource(Resource<'py>),
    /// A path reference to a resource (e.g., "myservice.db").
    Path(String),
}
