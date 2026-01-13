//! API-related resource types and parsers.
//!
//! This module contains resources related to API definitions:
//! - Endpoints (API handlers)
//! - Auth handlers
//! - Gateways
//! - Services
//! - Service clients

pub mod api;
pub mod authhandler;
pub mod gateway;
pub mod service;
pub mod service_client;

// Re-export types
pub use api::{Access, Endpoint, EndpointType, Method};
pub use authhandler::AuthHandler;
pub use gateway::Gateway;
pub use service::Service;
pub use service_client::ServiceClient;

// Re-export parsers
pub use api::ENDPOINT_PARSER;
pub use authhandler::AUTHHANDLER_PARSER;
pub use gateway::GATEWAY_PARSER;
pub use service::SERVICE_PARSER;
