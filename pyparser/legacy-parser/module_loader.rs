//! Module loading and caching for Python source files.
//!
//! This module provides functionality for loading Python modules, parsing them,
//! and caching the results for efficient reuse.

use std::cell::{OnceCell, RefCell};
use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::sync::Arc;

use anyhow::Result;
use ruff_python_ast as ast;
use ruff_python_parser::{parse_module, ParseError};

use crate::parser::fileset::{FileId, FilePath, FileSet, Range, SourceFile};

/// A unique identifier for a module.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ModuleId(u32);

impl ModuleId {
    /// Returns the raw module ID value.
    pub fn raw(&self) -> u32 {
        self.0
    }
}

/// Represents a parsed Python module.
#[derive(Debug)]
pub struct Module {
    /// Unique module identifier.
    pub id: ModuleId,
    /// The file this module was loaded from.
    pub file_id: FileId,
    /// The file path.
    pub file_path: FilePath,
    /// The parsed AST.
    pub ast: ast::ModModule,
    /// Parse errors encountered during parsing.
    pub errors: Vec<ParseError>,
    /// The source code.
    pub source: Arc<str>,
    /// Cached import statements.
    cached_imports: OnceCell<Vec<ImportInfo>>,
    /// Cached top-level assignments.
    cached_assignments: OnceCell<Vec<AssignmentInfo>>,
}

impl Module {
    /// Returns all import statements in this module.
    pub fn imports(&self) -> &[ImportInfo] {
        self.cached_imports.get_or_init(|| {
            let mut imports = Vec::new();
            for stmt in &self.ast.body {
                match stmt {
                    ast::Stmt::Import(import) => {
                        for alias in &import.names {
                            imports.push(ImportInfo {
                                module_path: alias.name.to_string(),
                                name: alias
                                    .asname
                                    .as_ref()
                                    .map(|n| n.to_string())
                                    .unwrap_or_else(|| alias.name.to_string()),
                                is_from: false,
                                range: Range::from_text_range(
                                    FileId(self.file_id.raw()),
                                    alias.range,
                                ),
                            });
                        }
                    }
                    ast::Stmt::ImportFrom(import) => {
                        let module_path = import
                            .module
                            .as_ref()
                            .map(|m| m.to_string())
                            .unwrap_or_default();
                        for alias in &import.names {
                            let full_name = if alias.name.as_str() == "*" {
                                "*".to_string()
                            } else {
                                alias.name.to_string()
                            };
                            imports.push(ImportInfo {
                                module_path: module_path.clone(),
                                name: alias
                                    .asname
                                    .as_ref()
                                    .map(|n| n.to_string())
                                    .unwrap_or(full_name.clone()),
                                is_from: true,
                                range: Range::from_text_range(
                                    FileId(self.file_id.raw()),
                                    alias.range,
                                ),
                            });
                        }
                    }
                    _ => {}
                }
            }
            imports
        })
    }

    /// Returns top-level assignments in this module.
    pub fn assignments(&self) -> &[AssignmentInfo] {
        self.cached_assignments.get_or_init(|| {
            let mut assignments = Vec::new();
            for stmt in &self.ast.body {
                match stmt {
                    ast::Stmt::Assign(assign) => {
                        for target in &assign.targets {
                            if let ast::Expr::Name(name) = target {
                                assignments.push(AssignmentInfo {
                                    name: name.id.to_string(),
                                    value: assign.value.clone(),
                                    range: Range::from_text_range(
                                        FileId(self.file_id.raw()),
                                        assign.range,
                                    ),
                                });
                            }
                        }
                    }
                    ast::Stmt::AnnAssign(assign) => {
                        if let ast::Expr::Name(name) = &*assign.target {
                            if let Some(value) = &assign.value {
                                assignments.push(AssignmentInfo {
                                    name: name.id.to_string(),
                                    value: value.clone(),
                                    range: Range::from_text_range(
                                        FileId(self.file_id.raw()),
                                        assign.range,
                                    ),
                                });
                            }
                        }
                    }
                    _ => {}
                }
            }
            assignments
        })
    }

    /// Returns function definitions decorated with a specific decorator.
    pub fn decorated_functions(&self, decorator_name: &str) -> Vec<DecoratedFunction> {
        let mut functions = Vec::new();
        for stmt in &self.ast.body {
            if let ast::Stmt::FunctionDef(func) = stmt {
                for dec in &func.decorator_list {
                    if Self::decorator_matches(dec, decorator_name) {
                        functions.push(DecoratedFunction {
                            name: func.name.to_string(),
                            func: func.clone(),
                            decorator: dec.clone(),
                            range: Range::from_text_range(
                                FileId(self.file_id.raw()),
                                func.range,
                            ),
                        });
                        break;
                    }
                }
            }
        }
        functions
    }

    /// Checks if a decorator matches a given name (e.g., "api" or "api.raw").
    fn decorator_matches(decorator: &ast::Decorator, name: &str) -> bool {
        match &decorator.expression {
            ast::Expr::Name(n) => n.id.as_str() == name,
            ast::Expr::Attribute(attr) => {
                // Handle e.g., api.raw
                let full = Self::get_attribute_chain(attr);
                full == name
            }
            ast::Expr::Call(call) => {
                // Handle @api() or @api.raw()
                match &*call.func {
                    ast::Expr::Name(n) => n.id.as_str() == name,
                    ast::Expr::Attribute(attr) => {
                        let full = Self::get_attribute_chain(attr);
                        full == name
                    }
                    _ => false,
                }
            }
            _ => false,
        }
    }

    /// Gets the full dotted name from an attribute expression.
    fn get_attribute_chain(attr: &ast::ExprAttribute) -> String {
        let mut parts = vec![attr.attr.to_string()];
        let mut current: &ast::Expr = &attr.value;
        loop {
            match current {
                ast::Expr::Name(n) => {
                    parts.push(n.id.to_string());
                    break;
                }
                ast::Expr::Attribute(a) => {
                    parts.push(a.attr.to_string());
                    current = &a.value;
                }
                _ => break,
            }
        }
        parts.reverse();
        parts.join(".")
    }
}

/// Information about an import statement.
#[derive(Debug, Clone)]
pub struct ImportInfo {
    /// The module path being imported (e.g., "encoredev.api").
    pub module_path: String,
    /// The local name (as bound in this module).
    pub name: String,
    /// Whether this is a `from ... import` statement.
    pub is_from: bool,
    /// Source range.
    pub range: Range,
}

/// Information about a top-level assignment.
#[derive(Debug, Clone)]
pub struct AssignmentInfo {
    /// The variable name.
    pub name: String,
    /// The assigned value expression.
    pub value: Box<ast::Expr>,
    /// Source range.
    pub range: Range,
}

/// A function with a decorator.
#[derive(Debug, Clone)]
pub struct DecoratedFunction {
    /// The function name.
    pub name: String,
    /// The function definition.
    pub func: ast::StmtFunctionDef,
    /// The matching decorator.
    pub decorator: ast::Decorator,
    /// Source range.
    pub range: Range,
}

/// Module loader that manages loading and caching of Python modules.
pub struct ModuleLoader {
    /// The file set for source tracking.
    file_set: Arc<FileSet>,
    /// The application root directory.
    app_root: PathBuf,
    /// Cache of loaded modules by file path.
    modules_by_path: RefCell<HashMap<FilePath, Arc<Module>>>,
    /// Cache of loaded modules by ID.
    modules_by_id: RefCell<HashMap<ModuleId, Arc<Module>>>,
    /// Counter for generating module IDs.
    next_module_id: RefCell<u32>,
}

impl ModuleLoader {
    /// Creates a new module loader.
    pub fn new(file_set: Arc<FileSet>, app_root: PathBuf) -> Self {
        ModuleLoader {
            file_set,
            app_root,
            modules_by_path: RefCell::new(HashMap::new()),
            modules_by_id: RefCell::new(HashMap::new()),
            next_module_id: RefCell::new(1),
        }
    }

    /// Returns the file set.
    pub fn file_set(&self) -> &FileSet {
        &self.file_set
    }

    /// Returns the application root.
    pub fn app_root(&self) -> &Path {
        &self.app_root
    }

    /// Loads a module from a file path.
    pub fn load_file(&self, path: &Path) -> Result<Arc<Module>> {
        let file_path = FilePath::Real(path.to_path_buf());

        // Check cache first
        {
            let cache = self.modules_by_path.borrow();
            if let Some(module) = cache.get(&file_path) {
                return Ok(Arc::clone(module));
            }
        }

        // Load and parse the file
        let source_file = self.file_set.load_file(path)?;
        self.parse_module(source_file)
    }

    /// Loads a module from source content.
    pub fn load_source(&self, name: &str, content: String) -> Result<Arc<Module>> {
        let source_file = self.file_set.new_source_file(name.to_string(), content)?;
        self.parse_module(source_file)
    }

    /// Parses a source file into a module.
    fn parse_module(&self, source_file: Arc<SourceFile>) -> Result<Arc<Module>> {
        let file_path = source_file.path.clone();

        // Check cache again (in case of concurrent load)
        {
            let cache = self.modules_by_path.borrow();
            if let Some(module) = cache.get(&file_path) {
                return Ok(Arc::clone(module));
            }
        }

        // Parse the source
        let source: Arc<str> = source_file.content.clone().into();
        let parsed = parse_module(&source);

        // Extract the module AST and errors
        let (ast, errors) = match parsed {
            Ok(module) => (module.into_syntax(), Vec::new()),
            Err(parse_error) => {
                // Return the parse error
                return Err(anyhow::anyhow!("parse error: {:?}", parse_error));
            }
        };

        // Generate module ID
        let module_id = {
            let mut id = self.next_module_id.borrow_mut();
            let current = *id;
            *id += 1;
            ModuleId(current)
        };

        let module = Arc::new(Module {
            id: module_id,
            file_id: source_file.id,
            file_path: file_path.clone(),
            ast,
            errors,
            source,
            cached_imports: OnceCell::new(),
            cached_assignments: OnceCell::new(),
        });

        // Cache the module
        {
            let mut by_path = self.modules_by_path.borrow_mut();
            let mut by_id = self.modules_by_id.borrow_mut();
            by_path.insert(file_path, Arc::clone(&module));
            by_id.insert(module_id, Arc::clone(&module));
        }

        Ok(module)
    }

    /// Looks up a module by ID.
    pub fn get_module(&self, id: ModuleId) -> Option<Arc<Module>> {
        let cache = self.modules_by_id.borrow();
        cache.get(&id).cloned()
    }

    /// Looks up a module by file path.
    pub fn get_module_by_path(&self, path: &FilePath) -> Option<Arc<Module>> {
        let cache = self.modules_by_path.borrow();
        cache.get(path).cloned()
    }

    /// Resolves a Python import path to a file path.
    ///
    /// This handles:
    /// - Relative imports within the app
    /// - Package imports (encoredev.*)
    pub fn resolve_import(
        &self,
        module_path: &str,
        from_file: &Path,
    ) -> Option<PathBuf> {
        // Check for encoredev imports (these are runtime imports, not parsed)
        if module_path.starts_with("encoredev.") {
            return None;
        }

        // Convert module path to relative file path
        let parts: Vec<&str> = module_path.split('.').collect();
        if parts.is_empty() {
            return None;
        }

        // Try as a module file
        let mut module_file = self.app_root.clone();
        for part in &parts {
            module_file.push(part);
        }
        module_file.set_extension("py");
        if module_file.exists() {
            return Some(module_file);
        }

        // Try as a package __init__.py
        module_file.set_extension("");
        module_file.push("__init__.py");
        if module_file.exists() {
            return Some(module_file);
        }

        // Try relative to the current file
        if let Some(parent) = from_file.parent() {
            let mut rel_file = parent.to_path_buf();
            for part in &parts {
                rel_file.push(part);
            }
            rel_file.set_extension("py");
            if rel_file.exists() {
                return Some(rel_file);
            }
        }

        None
    }

    /// Returns all loaded modules.
    pub fn all_modules(&self) -> Vec<Arc<Module>> {
        let cache = self.modules_by_id.borrow();
        cache.values().cloned().collect()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_import_parsing() {
        let file_set = Arc::new(FileSet::new());
        let loader = ModuleLoader::new(file_set, PathBuf::from("/tmp"));

        let source = r#"
from encoredev.api import api
from encoredev.service import Service
import os
import sys as system

svc = Service("my-service")
"#;

        let module = loader.load_source("test.py", source.to_string()).unwrap();
        let imports = module.imports();

        assert_eq!(imports.len(), 4);
        assert_eq!(imports[0].module_path, "encoredev.api");
        assert_eq!(imports[0].name, "api");
        assert!(imports[0].is_from);

        assert_eq!(imports[1].module_path, "encoredev.service");
        assert_eq!(imports[1].name, "Service");

        assert_eq!(imports[2].module_path, "os");
        assert_eq!(imports[2].name, "os");
        assert!(!imports[2].is_from);

        assert_eq!(imports[3].module_path, "sys");
        assert_eq!(imports[3].name, "system");
    }

    #[test]
    fn test_assignment_parsing() {
        let file_set = Arc::new(FileSet::new());
        let loader = ModuleLoader::new(file_set, PathBuf::from("/tmp"));

        let source = r#"
from encoredev.service import Service

svc = Service("my-service")
db = SQLDatabase("mydb")
"#;

        let module = loader.load_source("test.py", source.to_string()).unwrap();
        let assignments = module.assignments();

        assert_eq!(assignments.len(), 2);
        assert_eq!(assignments[0].name, "svc");
        assert_eq!(assignments[1].name, "db");
    }
}
