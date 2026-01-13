pub mod errors;
pub mod fileset;
pub mod modpath;
pub mod module_resolver;
pub mod parsectx;

use std::sync::{atomic::AtomicU32, Arc};

pub use ast::name::{QualifiedName, QualifiedNameBuilder};
use memo_map::MemoMap;
use ruff_python_ast as ast;

use crate::ast::loader::{
    errors::{ParseError, ParseResult},
    fileset::{File, FilePath, FileSet},
    modpath::ModulePath,
    module_resolver::ModuleResolver,
};

/// Loader loads Python modules and tracks them.
pub struct ModuleLoader {
    next_module_id: AtomicU32,
    resolver: Arc<dyn ModuleResolver>,
    file_set: Arc<FileSet>,
    modules: MemoMap<ModulePath, Module>,
}

impl std::fmt::Debug for ModuleLoader {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ModuleLoader")
            .field("next_module_id", &self.next_module_id)
            .finish_non_exhaustive()
    }
}

impl ModuleLoader {
    pub fn new(resolver: Arc<dyn ModuleResolver>, file_set: Arc<FileSet>) -> Self {
        ModuleLoader {
            next_module_id: AtomicU32::new(1),
            resolver,
            file_set,
            modules: MemoMap::default(),
        }
    }

    /// Gets the module corresponding to the given python module path.
    pub fn get_or_resolve<'py>(&'py self, mod_path: &ModulePath) -> ParseResult<&'py Module> {
        self.modules
            .get_or_try_insert(mod_path, || -> ParseResult<Module> {
                let info = self.resolver.resolve_module(mod_path.to_owned())?;
                let file_path = FilePath::Real(info.file_path);
                self.parse_module(mod_path.to_owned(), file_path)
            })
    }

    pub fn get_or_inject<'py>(
        &'py self,
        mod_path: &'_ ModulePath,
        file_path: FilePath,
    ) -> ParseResult<&'py Module> {
        self.modules
            .get_or_try_insert(mod_path, || -> ParseResult<Module> {
                self.parse_module(mod_path.to_owned(), file_path)
            })
    }

    fn parse_module(&self, mod_path: ModulePath, file_path: FilePath) -> ParseResult<Module> {
        let file = self
            .file_set
            .get(&file_path)
            .map_err(|err| ParseError::IO {
                file_path: Some(file_path.to_owned()),
                message: err.to_string(),
            })?;

        let parsed = ruff_python_parser::parse_unchecked_source(
            &file.contents,
            ruff_python_ast::PySourceType::Python,
        );

        for err in parsed.errors() {
            // TODO report multiple errors
            return Err(ParseError::Parse {
                span: file.id.to_span(err.location),
                message: err.to_string(),
            });
        }

        let id = ModuleId(
            self.next_module_id
                .fetch_add(1, std::sync::atomic::Ordering::SeqCst),
        );

        Ok(Module::new(id, mod_path, file, parsed.into_syntax()))
    }
}

#[derive(Debug)]
pub struct Module {
    pub id: ModuleId,
    pub mod_path: ModulePath,

    /// The package path for this module.
    /// For `__init__.py` files, this is the same as `mod_path`.
    /// For regular `.py` files, this is the parent of `mod_path`.
    /// For files outside of a module, it is None.
    pub pkg_path: Option<ModulePath>,

    pub is_package: bool,

    pub file: Arc<File>,
    pub ast: ast::ModModule,
}

#[derive(Clone, Debug, PartialOrd, Ord, PartialEq, Eq, Hash, Copy)]
pub struct ModuleId(u32);

impl ModuleId {
    /// Creates a new ModuleId for testing purposes.
    #[cfg(test)]
    pub fn new_for_test(id: u32) -> Self {
        ModuleId(id)
    }
}

impl Module {
    fn new(id: ModuleId, mod_path: ModulePath, file: Arc<File>, ast: ast::ModModule) -> Self {
        let is_package = file.path.file_name() == Some("__init__.py");
        let pkg_path = if is_package {
            Some(mod_path.clone())
        } else {
            mod_path.parent()
        };

        Module {
            id,
            mod_path,
            pkg_path,
            file,
            is_package,
            ast,
        }
    }
}
