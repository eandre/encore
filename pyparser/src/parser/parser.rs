//! Main parser entry point for Python applications.
//!
//! This module provides the main parsing functionality that coordinates
//! module loading, resource discovery, and service identification.

use std::path::{Path, PathBuf};
use std::sync::Arc;

use walkdir::WalkDir;

use crate::ast::loader::errors::{ParseError, ParseResult};
use crate::ast::loader::fileset::{FilePath, FileSet};
use crate::ast::loader::modpath::ModulePath;
use crate::ast::loader::{parsectx, ModuleLoader};
use crate::ast::schema::Parser as SchemaParser;
use crate::parser::resourceparser::PassOneParser;
use crate::parser::resources::{Bind, Resource};
use crate::parser::service_discovery::{discover_services, find_containing_service};
use crate::parser::DiscoveredService;

#[derive(Debug)]
pub struct ParsedApp<'py> {
    /// Discovered resources.
    pub resources: Vec<Resource<'py>>,
    /// Resource bindings.
    pub binds: Vec<Arc<Bind<'py>>>,
    /// Discovered services.
    pub services: Vec<DiscoveredService>,
}

pub fn parse<'py>(
    ctx: &'py parsectx::Context,
    loader: &'py ModuleLoader,
    file_set: Arc<FileSet>,
    schema_parser: &'py SchemaParser<'py>,
) -> ParseResult<ParsedApp<'py>> {
    let mut pass1 = PassOneParser::new(schema_parser);

    let mut all_resources: Vec<Resource<'py>> = Vec::new();
    let mut all_binds: Vec<Arc<Bind<'py>>> = Vec::new();

    // Collect Python files to parse.
    // First process encore_service.py files.
    let files = collect_files(ctx)?;
    let (service_files, other_files): (Vec<_>, Vec<_>) = files.into_iter().partition(|f| {
        f.path
            .file_name()
            .map(|n| n == "encore_service.py")
            .unwrap_or(false)
    });

    // Parse service files first
    for app_file in service_files {
        let module = loader.get_or_inject(&app_file.mod_path, FilePath::Real(app_file.path))?;
        let (resources, binds) = pass1.parse_module(module, None)?;
        all_resources.extend(resources);
        all_binds.extend(binds.into_iter().map(Arc::new));
    }

    // Build initial service map
    let mut services = discover_services(&file_set, &all_binds, &ctx.app_root);

    for app_file in other_files {
        // Find the service this file belongs to
        let service_name = find_containing_service(&services, &app_file.path, &ctx.app_root)
            .map(|s| s.name.clone());

        let module = loader.get_or_inject(&app_file.mod_path, FilePath::Real(app_file.path))?;
        let (resources, binds) = pass1.parse_module(module, service_name)?;
        all_resources.extend(resources);
        all_binds.extend(binds.into_iter().map(Arc::new));
    }

    // Re-discover services with all resources
    services = discover_services(&file_set, &all_binds, &ctx.app_root);

    Ok(ParsedApp {
        resources: all_resources,
        binds: all_binds,
        services,
    })
}

struct AppModuleFile {
    path: PathBuf,
    mod_path: ModulePath,
}

/// Collects Python files to parse from the application root.
fn collect_files(pc: &parsectx::Context) -> ParseResult<Vec<AppModuleFile>> {
    let mut files = Vec::new();

    for entry in WalkDir::new(&pc.app_root)
        .follow_links(true)
        // Sort files, putting encore.service.py files first
        .sort_by(|a, b| {
            let (a_name, b_name) = (a.file_name(), b.file_name());
            let a_is_service = a_name == "encore_service.py";
            let b_is_service = b_name == "encore_service.py";

            match (a_is_service, b_is_service) {
                (true, false) => std::cmp::Ordering::Less,
                (false, true) => std::cmp::Ordering::Greater,
                _ => a_name.cmp(b_name),
            }
        })
        .into_iter()
        .filter_entry(|e| !should_skip(e))
    {
        let entry = entry.map_err(|err| ParseError::IO {
            file_path: err.path().map(|p| FilePath::Real(p.to_path_buf())),
            message: err.to_string(),
        })?;

        let path = entry.path();

        // Only process .py files
        if path.is_file() && path.extension().map(|e| e == "py").unwrap_or(false) {
            // Skip test files
            if is_test_file(path) {
                continue;
            }

            let Some(mod_path) = compute_module_path(path, &pc.app_root) else {
                continue; // Skip files outside app_root
            };

            files.push(AppModuleFile {
                path: path.to_path_buf(),
                mod_path,
            });
        }
    }

    Ok(files)
}

/// Computes the Python module path from a file path.
///
/// Converts `foo/bar/baz.py` to `foo.bar.baz` and
/// `foo/bar/__init__.py` to `foo.bar`.
fn compute_module_path(path: &Path, app_root: &Path) -> Option<ModulePath> {
    let rel_path = path.strip_prefix(app_root).ok()?;

    // Remove the .py extension
    let without_ext = rel_path.with_extension("");

    // Convert path components to module path
    let mut components: Vec<_> = without_ext
        .components()
        .filter_map(|c| c.as_os_str().to_str())
        .collect();

    if components.last() == Some(&"__init__") {
        components.pop();
    }
    ModulePath::from_segments(components)
}

/// Checks if a directory entry should be skipped.
fn should_skip(entry: &walkdir::DirEntry) -> bool {
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
fn is_test_file(path: &Path) -> bool {
    let name = path.file_name().and_then(|n| n.to_str()).unwrap_or("");

    // Common test file patterns
    name.starts_with("test_")
        || name.ends_with("_test.py")
        || name == "conftest.py"
        || path
            .components()
            .any(|c| c.as_os_str() == "tests" || c.as_os_str() == "test")
}
