//! Main parser entry point for Python applications.
//!
//! This module provides the main parsing functionality that coordinates
//! module loading, resource discovery, and service identification.

use std::path::{Path, PathBuf};
use std::sync::Arc;

use anyhow::{Context, Result};
use walkdir::WalkDir;

use crate::parser::fileset::FileSet;
use crate::parser::module_loader::ModuleLoader;
use crate::parser::resourceparser::PassOneParser;
use crate::parser::resources::{Bind, Resource};
use crate::parser::service_discovery::{discover_services, find_containing_service, DiscoveredService};
use crate::parser::types::{ResolveState, TypeChecker};

/// Context for parsing that holds shared state.
pub struct ParseContext {
    /// The application root directory.
    pub app_root: PathBuf,
    /// The file set for source tracking.
    pub file_set: Arc<FileSet>,
    /// The module loader.
    pub loader: Arc<ModuleLoader>,
    /// The type resolution state.
    pub resolve_state: Arc<ResolveState>,
}

impl ParseContext {
    /// Creates a new parse context for the given application root.
    pub fn new(app_root: PathBuf) -> Result<Self> {
        let file_set = Arc::new(FileSet::new());
        let loader = Arc::new(ModuleLoader::new(Arc::clone(&file_set), app_root.clone()));
        let resolve_state = Arc::new(ResolveState::new());

        Ok(ParseContext {
            app_root,
            file_set,
            loader,
            resolve_state,
        })
    }

    /// Creates a TypeChecker for type resolution.
    pub fn type_checker(&self) -> TypeChecker<'_> {
        TypeChecker::new(&self.resolve_state)
    }
}

/// Result of parsing an application.
#[derive(Debug)]
pub struct ParseResult {
    /// Discovered resources.
    pub resources: Vec<Resource>,
    /// Resource bindings.
    pub binds: Vec<Arc<Bind>>,
    /// Discovered services.
    pub services: Vec<DiscoveredService>,
}

/// The main parser for Python Encore applications.
pub struct Parser<'a> {
    /// The parse context.
    pc: &'a ParseContext,
    /// The pass one parser.
    pass1: PassOneParser,
}

impl<'a> Parser<'a> {
    /// Creates a new parser.
    pub fn new(pc: &'a ParseContext) -> Self {
        Parser {
            pc,
            pass1: PassOneParser::new(Arc::clone(&pc.file_set), Arc::clone(&pc.resolve_state)),
        }
    }

    /// Parses the application and returns the result.
    pub fn parse(&mut self) -> Result<ParseResult> {
        let mut all_resources: Vec<Resource> = Vec::new();
        let mut all_binds: Vec<Arc<Bind>> = Vec::new();

        // Collect Python files to parse
        let files = self.collect_files()?;

        // First, parse encore.service.py files to discover explicit services
        let service_files: Vec<_> = files
            .iter()
            .filter(|f| {
                f.file_name()
                    .map(|n| n == "encore.service.py")
                    .unwrap_or(false)
            })
            .collect();

        // Parse service files first
        for path in &service_files {
            let module = self.pc.loader.load_file(path)?;
            let (resources, binds) = self.pass1.parse_module(module, None);
            all_resources.extend(resources);
            all_binds.extend(binds.into_iter().map(Arc::new));
        }

        // Build initial service map
        let mut services = discover_services(&self.pc.file_set, &all_binds, &self.pc.app_root);

        // Parse remaining files
        let other_files: Vec<_> = files
            .iter()
            .filter(|f| {
                !f.file_name()
                    .map(|n| n == "encore.service.py")
                    .unwrap_or(false)
            })
            .collect();

        for path in &other_files {
            let module = self.pc.loader.load_file(path)?;

            // Find the service this file belongs to
            let service_name =
                find_containing_service(&services, path, &self.pc.app_root).map(|s| s.name.clone());

            let (resources, binds) = self.pass1.parse_module(module, service_name);
            all_resources.extend(resources);
            all_binds.extend(binds.into_iter().map(Arc::new));
        }

        // Re-discover services with all resources
        services = discover_services(&self.pc.file_set, &all_binds, &self.pc.app_root);

        Ok(ParseResult {
            resources: all_resources,
            binds: all_binds,
            services,
        })
    }

    /// Collects Python files to parse from the application root.
    fn collect_files(&self) -> Result<Vec<PathBuf>> {
        let mut files = Vec::new();

        for entry in WalkDir::new(&self.pc.app_root)
            .follow_links(true)
            .into_iter()
            .filter_entry(|e| !self.should_skip(e))
        {
            let entry = entry.context("failed to read directory entry")?;
            let path = entry.path();

            // Only process .py files
            if path.is_file() && path.extension().map(|e| e == "py").unwrap_or(false) {
                // Skip test files
                if self.is_test_file(path) {
                    continue;
                }
                files.push(path.to_path_buf());
            }
        }

        // Sort files, putting encore.service.py files first
        files.sort_by(|a, b| {
            let a_is_service = a
                .file_name()
                .map(|n| n == "encore.service.py")
                .unwrap_or(false);
            let b_is_service = b
                .file_name()
                .map(|n| n == "encore.service.py")
                .unwrap_or(false);
            match (a_is_service, b_is_service) {
                (true, false) => std::cmp::Ordering::Less,
                (false, true) => std::cmp::Ordering::Greater,
                _ => a.cmp(b),
            }
        });

        Ok(files)
    }

    /// Checks if a directory entry should be skipped.
    fn should_skip(&self, entry: &walkdir::DirEntry) -> bool {
        // Never skip the root directory
        if entry.depth() == 0 {
            return false;
        }

        let name = entry.file_name().to_string_lossy();

        // Skip hidden files/directories
        if name.starts_with('.') {
            return true;
        }

        // Skip common directories to ignore
        let skip_dirs = [
            "__pycache__",
            "venv",
            "node_modules",
            "encore_gen",
            "dist",
            "build",
        ];

        if entry.file_type().is_dir() {
            for skip in &skip_dirs {
                if name == *skip || (skip.starts_with('*') && name.ends_with(&skip[1..])) {
                    return true;
                }
            }
        }

        false
    }

    /// Checks if a file is a test file.
    fn is_test_file(&self, path: &Path) -> bool {
        let name = path
            .file_name()
            .and_then(|n| n.to_str())
            .unwrap_or("");

        // Common test file patterns
        name.starts_with("test_")
            || name.ends_with("_test.py")
            || name == "conftest.py"
            || path
                .components()
                .any(|c| c.as_os_str() == "tests" || c.as_os_str() == "test")
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;
    use tempfile::TempDir;

    fn setup_test_app() -> (TempDir, PathBuf) {
        let dir = TempDir::new().unwrap();
        let root = dir.path().to_path_buf();

        // Create a simple service
        let svc_dir = root.join("users");
        fs::create_dir_all(&svc_dir).unwrap();

        // encore.service.py
        fs::write(
            svc_dir.join("encore.service.py"),
            r#"
from encoredev.service import Service

svc = Service("users")
"#,
        )
        .unwrap();

        // endpoints.py
        fs::write(
            svc_dir.join("endpoints.py"),
            r#"
from encoredev.api import api

@api(method="GET", path="/users/:id")
async def get_user(id: int) -> dict:
    return {"id": id}

@api(method="POST", path="/users")
async def create_user(name: str) -> dict:
    return {"name": name}
"#,
        )
        .unwrap();

        (dir, root)
    }

    #[test]
    fn test_parse_simple_app() {
        let (_dir, root) = setup_test_app();
        let pc = ParseContext::new(root).unwrap();
        let mut parser = Parser::new(&pc);
        let result = parser.parse().unwrap();

        // Should find the service
        assert_eq!(result.services.len(), 1);
        assert_eq!(result.services[0].name, "users");

        // Should find 3 resources: 1 service + 2 endpoints
        assert_eq!(result.resources.len(), 3);
    }

    #[test]
    fn test_skip_test_files() {
        let dir = TempDir::new().unwrap();
        let root = dir.path().to_path_buf();

        fs::create_dir_all(root.join("svc")).unwrap();
        fs::write(
            root.join("svc/encore.service.py"),
            "from encoredev.service import Service\nsvc = Service('svc')",
        )
        .unwrap();
        fs::write(root.join("svc/test_api.py"), "# test file").unwrap();
        fs::write(root.join("svc/api_test.py"), "# test file").unwrap();
        fs::write(root.join("svc/conftest.py"), "# conftest").unwrap();

        let pc = ParseContext::new(root).unwrap();
        let parser = Parser::new(&pc);
        let files = parser.collect_files().unwrap();

        // Should only include encore.service.py, not test files
        assert_eq!(files.len(), 1);
        assert!(files[0].ends_with("encore.service.py"));
    }
}
