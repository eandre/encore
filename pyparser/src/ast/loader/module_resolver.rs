use std::path::PathBuf;

use thiserror::Error;

use crate::ast::loader::modpath::ModulePath;

/// Resolves module paths to file paths.
pub trait ModuleResolver: Send + Sync {
    /// Resolves the module at the given path.
    fn resolve_module(&self, mod_path: ModulePath) -> Result<ModuleInfo, ResolveError>;
}

pub struct ModuleInfo {
    pub mod_path: ModulePath,
    pub pkg_path: ModulePath,
    pub file_path: PathBuf,
    pub is_package_init: bool,
}

pub struct DefaultModuleResolver {
    roots: Vec<PathBuf>,
}

impl DefaultModuleResolver {
    pub fn new(roots: Vec<PathBuf>) -> Self {
        Self { roots }
    }
}

#[derive(Error, Clone, Debug)]
pub enum ResolveError {
    #[error("module not found")]
    NotFound,
    #[error("io error: {0}")]
    IO(String),
}

impl ModuleResolver for DefaultModuleResolver {
    fn resolve_module(&self, mod_path: ModulePath) -> Result<ModuleInfo, ResolveError> {
        let mut segments: Vec<_> = mod_path.segments().collect();

        // Look for each candidate in each root
        for root in &self.roots {
            let mut candidate = root.clone();
            candidate.extend(&segments);
            // Firts try "path/to/mod/__init__.py"
            candidate.push("__init__.py");

            if candidate.is_file() {
                return Ok(ModuleInfo {
                    mod_path: mod_path.to_owned(),
                    pkg_path: mod_path.to_owned(),
                    file_path: candidate,
                    is_package_init: true,
                });
            }

            // Now try "path/to/mod.py"
            candidate.pop();
            candidate.set_extension("py");
            if candidate.is_file() {
                // Compute package name (all segments except the last)
                let pkg_name = {
                    segments.pop();
                    ModulePath::from_segments(segments).ok_or(ResolveError::NotFound)?
                };

                return Ok(ModuleInfo {
                    mod_path: mod_path.to_owned(),
                    pkg_path: pkg_name,
                    file_path: candidate,
                    is_package_init: false,
                });
            }
        }

        Err(ResolveError::NotFound)
    }
}
