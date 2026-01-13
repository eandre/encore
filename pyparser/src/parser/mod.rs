//! Python parser module for Encore applications.
//!
//! This module provides functionality for parsing Python source files
//! and discovering Encore resources, services, and API endpoints.

pub mod doc_comments;
pub mod parser;
pub mod resourceparser;
pub mod resources;
pub mod respath;
pub mod service_discovery;

// Re-export key types
pub use service_discovery::{discover_services, DiscoveredService};
