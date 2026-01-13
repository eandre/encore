//! Service client resource definition.
//!
//! Service clients represent references to other services for making
//! service-to-service calls.

use crate::ast::loader::fileset::Span;

/// A service client definition.
#[derive(Debug, Clone)]
pub struct ServiceClient {
    /// The name of the service being called.
    pub service_name: String,
    /// Source range of the client definition.
    pub span: Span,
    /// Documentation comment.
    pub doc: Option<String>,
}
