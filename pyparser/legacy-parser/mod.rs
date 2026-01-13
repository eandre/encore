//! Python parser module for Encore applications.
//!
//! This module provides functionality for parsing Python source files
//! and discovering Encore resources, services, and API endpoints.

pub mod doc_comments;
pub mod fileset;
pub mod module_loader;
pub mod parser;
pub mod resources;
pub mod resourceparser;
pub mod respath;
pub mod service_discovery;
pub mod types;

// Re-export key types
pub use fileset::{FileSet, FilePath, Pos, Range};
pub use module_loader::{Module, ModuleId, ModuleLoader};
pub use parser::{ParseContext, ParseResult, Parser};
pub use resourceparser::{PassOneParser, ResourceParseContext};
pub use resources::{Bind, BindKind, Resource};
pub use service_discovery::{discover_services, DiscoveredService};
