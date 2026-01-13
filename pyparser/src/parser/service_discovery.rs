//! Service discovery for Python applications.
//!
//! This module discovers service boundaries based on:
//! - Explicit Service() definitions in encore.service.py files
//! - Implicit service roots from resource definitions

use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::sync::Arc;

use crate::ast::loader::fileset::FileSet;
use crate::parser::resources::{Bind, Resource};

/// A discovered service.
#[derive(Debug, Clone)]
pub struct DiscoveredService {
    /// The service name.
    pub name: String,
    /// The root directory of the service.
    pub root: PathBuf,
    /// Whether this was explicitly defined (via Service()).
    pub explicit: bool,
    /// Documentation comment (if available).
    pub doc: Option<String>,
}

/// Root type for service discovery.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum RootType {
    /// Strong root from explicit Service() definition.
    Strong,
    /// Weak root from other resources (endpoints, subscriptions, etc.).
    Weak,
}

/// A potential service root.
struct ServiceRoot {
    name: String,
    path: PathBuf,
    root_type: RootType,
    doc: Option<String>,
}

/// Discovers services from the parsed binds.
pub fn discover_services(
    file_set: &FileSet,
    binds: &[Arc<Bind>],
    app_root: &Path,
) -> Vec<DiscoveredService> {
    let mut roots: Vec<ServiceRoot> = Vec::new();
    let mut path_to_service: HashMap<PathBuf, String> = HashMap::new();

    // First pass: collect all potential service roots
    for bind in binds {
        let span = bind.span;
        if !span.is_valid() {
            continue;
        }

        let file = file_set.get_by_id(span.file_id());
        let Some(file_dir) = file.path.as_real().and_then(|p| p.parent()) else {
            continue;
        };

        // Make path relative to app root
        let rel_path = file_dir
            .strip_prefix(app_root)
            .unwrap_or(file_dir)
            .to_path_buf();

        match &bind.resource {
            Resource::Service(svc) => {
                // Strong root from explicit Service() definition
                roots.push(ServiceRoot {
                    name: svc.name.clone(),
                    path: rel_path.clone(),
                    root_type: RootType::Strong,
                    doc: svc.doc.clone(),
                });
                path_to_service.insert(rel_path, svc.name.clone());
            }
            Resource::APIEndpoint(_)
            | Resource::PubSubSubscription(_)
            | Resource::Gateway(_)
            | Resource::AuthHandler(_) => {
                // Weak root from resources that define service boundaries
                // Only add if there's no service already defined for this path
                if !path_to_service.contains_key(&rel_path) {
                    // Derive service name from directory
                    let name = derive_service_name(&rel_path);
                    roots.push(ServiceRoot {
                        name: name.clone(),
                        path: rel_path.clone(),
                        root_type: RootType::Weak,
                        doc: None,
                    });
                    path_to_service.insert(rel_path, name);
                }
            }
            _ => {
                // Other resources don't create service roots
            }
        }
    }

    // Second pass: merge and deduplicate roots
    let mut services = merge_roots(roots);

    // Sort by name for deterministic output
    services.sort_by(|a, b| a.name.cmp(&b.name));

    services
}

/// Merges service roots, handling overlapping weak roots.
fn merge_roots(roots: Vec<ServiceRoot>) -> Vec<DiscoveredService> {
    if roots.is_empty() {
        return Vec::new();
    }

    // Group roots by path
    let mut by_path: HashMap<PathBuf, Vec<ServiceRoot>> = HashMap::new();
    for root in roots {
        by_path.entry(root.path.clone()).or_default().push(root);
    }

    // For each path, select the best root (prefer strong over weak)
    let mut selected: Vec<ServiceRoot> = Vec::new();
    for (path, mut path_roots) in by_path {
        // Sort so strong roots come first
        path_roots.sort_by(|a, b| match (a.root_type, b.root_type) {
            (RootType::Strong, RootType::Weak) => std::cmp::Ordering::Less,
            (RootType::Weak, RootType::Strong) => std::cmp::Ordering::Greater,
            _ => std::cmp::Ordering::Equal,
        });

        // Take the first (strongest) root
        if let Some(root) = path_roots.into_iter().next() {
            selected.push(root);
        }
    }

    // Sort by path depth (shortest first) for parent checking
    selected.sort_by(|a, b| {
        let a_depth = a.path.components().count();
        let b_depth = b.path.components().count();
        a_depth.cmp(&b_depth)
    });

    // Remove weak roots that are descendants of other roots
    let mut final_roots: Vec<ServiceRoot> = Vec::new();
    for root in selected {
        // Check if this root is a descendant of any existing root
        let is_descendant = final_roots
            .iter()
            .any(|parent| root.path.starts_with(&parent.path) && root.path != parent.path);

        // Only add if not a descendant, or if it's a strong root
        if !is_descendant || root.root_type == RootType::Strong {
            // If it's a strong descendant, check for conflicts
            if is_descendant && root.root_type == RootType::Strong {
                // This is a nested service - allowed but should be noted
            }
            final_roots.push(root);
        }
    }

    // Convert to DiscoveredService
    final_roots
        .into_iter()
        .map(|root| DiscoveredService {
            name: root.name,
            root: root.path,
            explicit: root.root_type == RootType::Strong,
            doc: root.doc,
        })
        .collect()
}

/// Derives a service name from a directory path.
fn derive_service_name(path: &Path) -> String {
    // Use the last component of the path as the service name
    path.file_name()
        .and_then(|n| n.to_str())
        .map(|s| s.to_string())
        .unwrap_or_else(|| "default".to_string())
}

/// Finds the service that contains a given file path.
pub fn find_containing_service<'a>(
    services: &'a [DiscoveredService],
    file_path: &Path,
    app_root: &Path,
) -> Option<&'a DiscoveredService> {
    let rel_path = file_path.strip_prefix(app_root).unwrap_or(file_path);

    // Find the deepest service root that contains this path
    services
        .iter()
        .filter(|svc| rel_path.starts_with(&svc.root))
        .max_by_key(|svc| svc.root.components().count())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_derive_service_name() {
        assert_eq!(derive_service_name(Path::new("users")), "users");
        assert_eq!(derive_service_name(Path::new("api/users")), "users");
        assert_eq!(derive_service_name(Path::new("")), "default");
    }

    #[test]
    fn test_merge_strong_weak() {
        let roots = vec![
            ServiceRoot {
                name: "users".to_string(),
                path: PathBuf::from("users"),
                root_type: RootType::Strong,
                doc: None,
            },
            ServiceRoot {
                name: "users".to_string(),
                path: PathBuf::from("users"),
                root_type: RootType::Weak,
                doc: None,
            },
        ];

        let services = merge_roots(roots);
        assert_eq!(services.len(), 1);
        assert!(services[0].explicit);
    }
}
