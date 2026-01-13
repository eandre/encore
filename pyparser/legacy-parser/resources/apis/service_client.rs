//! Service client resource definition.
//!
//! Service clients represent references to other services for making
//! service-to-service calls.

use crate::parser::fileset::Range;

/// A service client definition.
#[derive(Debug, Clone)]
pub struct ServiceClient {
    /// The name of the service being called.
    pub service_name: String,
    /// Source range of the client definition.
    pub range: Range,
    /// Documentation comment.
    pub doc: Option<String>,
}

impl ServiceClient {
    /// Creates a new service client.
    pub fn new(service_name: String, range: Range) -> Self {
        ServiceClient {
            service_name,
            range,
            doc: None,
        }
    }
}
