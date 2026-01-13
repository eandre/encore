//! Pass one resource parsing.
//!
//! This module implements the first pass of parsing, which discovers
//! resources defined in Python source files.

pub mod parsers;

use std::collections::HashMap;
use std::sync::Arc;

use crate::parser::fileset::{FileId, FileSet, Range};
use crate::parser::module_loader::Module;
use crate::parser::module_loader::ModuleId as LoaderModuleId;
use crate::parser::resources::{Bind, BindId, BindKind, Resource};
use crate::parser::types::{ModuleId as TypeModuleId, ResolveState, TypeChecker};

/// Context for resource parsing within a module.
pub struct ResourceParseContext<'a> {
    /// The module being parsed.
    pub module: Arc<Module>,
    /// The file set for location tracking.
    file_set: &'a FileSet,
    /// The type resolution state.
    resolve_state: &'a ResolveState,
    /// The type system module ID for this module.
    type_module_id: TypeModuleId,
    /// Current service name (if known).
    pub service_name: Option<String>,
    /// Discovered resources.
    resources: Vec<Resource>,
    /// Created bindings.
    binds: Vec<Bind>,
    /// Next bind ID.
    next_bind_id: u32,
    /// Tracked imports: maps local name -> (module_path, original_name).
    imports: HashMap<String, (String, String)>,
}

impl<'a> ResourceParseContext<'a> {
    /// Creates a new resource parse context.
    pub fn new(module: Arc<Module>, file_set: &'a FileSet, resolve_state: &'a ResolveState) -> Self {
        // Build import map
        let mut imports = HashMap::new();
        for import in module.imports() {
            imports.insert(
                import.name.clone(),
                (import.module_path.clone(), import.name.clone()),
            );
        }

        // Create a type system module ID for this module
        let type_module_id = resolve_state.new_module_id();

        ResourceParseContext {
            module,
            file_set,
            resolve_state,
            type_module_id,
            service_name: None,
            resources: Vec::new(),
            binds: Vec::new(),
            next_bind_id: 1,
            imports,
        }
    }

    /// Creates a TypeChecker for type resolution.
    pub fn type_checker(&self) -> TypeChecker<'_> {
        TypeChecker::new(self.resolve_state)
    }

    /// Returns the type system module ID for this module.
    pub fn type_module_id(&self) -> TypeModuleId {
        self.type_module_id
    }

    /// Adds a discovered resource.
    pub fn add_resource(&mut self, resource: Resource) {
        self.resources.push(resource);
    }

    /// Adds a binding.
    pub fn add_bind(
        &mut self,
        resource: Resource,
        kind: BindKind,
        range: Range,
        name: Option<String>,
    ) {
        let id = BindId(self.next_bind_id);
        self.next_bind_id += 1;

        let bind = Bind::new(id, range, resource, kind, self.module.id, name);
        self.binds.push(bind);
    }

    /// Returns the loader module ID.
    pub fn module_id(&self) -> LoaderModuleId {
        self.module.id
    }

    /// Returns the file ID.
    pub fn file_id(&self) -> FileId {
        self.module.file_id
    }

    /// Creates a range from a text range.
    pub fn range(&self, text_range: ruff_text_size::TextRange) -> Range {
        Range::from_text_range(self.module.file_id, text_range)
    }

    /// Checks if a name is imported from a specific module.
    pub fn is_imported_from(&self, name: &str, module_path: &str) -> bool {
        log::info!("got imports: {:?}", self.imports);
        if let Some((path, _)) = self.imports.get(name) {
            return path == module_path;
        }
        false
    }

    /// Checks if a name is imported from any encoredev module.
    pub fn is_encoredev_import(&self, name: &str) -> bool {
        if let Some((path, _)) = self.imports.get(name) {
            return path.starts_with("encoredev.");
        }
        false
    }

    /// Returns the discovered resources.
    pub fn into_results(self) -> (Vec<Resource>, Vec<Bind>) {
        (self.resources, self.binds)
    }
}

/// A resource parser plugin.
pub struct ResourceParser {
    /// Parser name.
    pub name: &'static str,
    /// Module paths this parser is interested in.
    pub interesting_pkgs: &'static [&'static str],
    /// The parsing function.
    pub run: fn(&mut ResourceParseContext),
}

/// Registry of resource parsers.
pub struct ResourceParserRegistry {
    /// All registered parsers.
    parsers: Vec<&'static ResourceParser>,
    /// Map from package path to interested parsers.
    interested_for_paths: HashMap<&'static str, Vec<&'static ResourceParser>>,
}

impl ResourceParserRegistry {
    /// Creates a new registry with the default parsers.
    pub fn new() -> Self {
        let mut registry = ResourceParserRegistry {
            parsers: Vec::new(),
            interested_for_paths: HashMap::new(),
        };

        // Register all parsers
        for parser in parsers::DEFAULT_RESOURCE_PARSERS {
            registry.register(parser);
        }

        registry
    }

    /// Registers a parser.
    fn register(&mut self, parser: &'static ResourceParser) {
        self.parsers.push(parser);
        for pkg in parser.interesting_pkgs {
            self.interested_for_paths
                .entry(pkg)
                .or_insert_with(Vec::new)
                .push(parser);
        }
    }

    /// Returns parsers interested in the given import.
    pub fn parsers_for_import(&self, module_path: &str) -> Vec<&'static ResourceParser> {
        self.interested_for_paths
            .get(module_path)
            .cloned()
            .unwrap_or_default()
    }
}

impl Default for ResourceParserRegistry {
    fn default() -> Self {
        Self::new()
    }
}

/// First pass parser for resource discovery.
pub struct PassOneParser {
    /// The file set.
    file_set: Arc<FileSet>,
    /// The type resolution state.
    resolve_state: Arc<ResolveState>,
    /// Parser registry.
    registry: ResourceParserRegistry,
    /// Current bind ID counter.
    next_bind_id: u32,
}

impl PassOneParser {
    /// Creates a new pass one parser.
    pub fn new(file_set: Arc<FileSet>, resolve_state: Arc<ResolveState>) -> Self {
        PassOneParser {
            file_set,
            resolve_state,
            registry: ResourceParserRegistry::new(),
            next_bind_id: 1,
        }
    }

    /// Parses a module for resources.
    pub fn parse_module(
        &mut self,
        module: Arc<Module>,
        service_name: Option<String>,
    ) -> (Vec<Resource>, Vec<Bind>) {
        let mut ctx = ResourceParseContext::new(Arc::clone(&module), &self.file_set, &self.resolve_state);
        ctx.service_name = service_name;
        ctx.next_bind_id = self.next_bind_id;

        // Collect imports to determine which parsers to run
        let imported_modules: Vec<String> = module
            .imports()
            .iter()
            .map(|i| i.module_path.clone())
            .collect();

        // Run parsers that are interested in the imported modules
        let mut parsers_to_run: Vec<&'static ResourceParser> = Vec::new();
        for module_path in &imported_modules {
            for parser in self.registry.parsers_for_import(module_path) {
                // Compare by name since ResourceParser doesn't implement PartialEq
                let already_added = parsers_to_run.iter().any(|p| p.name == parser.name);
                if !already_added {
                    parsers_to_run.push(parser);
                }
            }
        }

        // Sort parsers to ensure service parser runs first
        parsers_to_run.sort_by(|a, b| {
            if a.name == "service" {
                std::cmp::Ordering::Less
            } else if b.name == "service" {
                std::cmp::Ordering::Greater
            } else {
                a.name.cmp(b.name)
            }
        });

        // Run the parsers
        for parser in parsers_to_run {
            (parser.run)(&mut ctx);
        }

        // Update bind ID counter
        self.next_bind_id = ctx.next_bind_id;

        ctx.into_results()
    }
}
