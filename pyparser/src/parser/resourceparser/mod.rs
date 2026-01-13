//! Pass one resource parsing.
//!
//! This module implements the first pass of parsing, which discovers
//! resources defined in Python source files.

pub mod parsers;

use std::collections::HashMap;

use ruff_text_size::Ranged;

use crate::ast::loader::errors::ParseResult;
use crate::ast::loader::fileset::Span;
use crate::ast::loader::modpath::ModulePath;
use crate::ast::loader::Module;
use crate::ast::schema::object::ModuleObj;
use crate::ast::schema::Parser as SchemaParser;
use crate::parser::resources::{Bind, BindId, BindKind, Resource};

/// Context for resource parsing within a module.
pub struct ResourceParseContext<'py> {
    pub resolver: &'py SchemaParser<'py>,

    /// The module being parsed.
    pub module_obj: &'py ModuleObj<'py>,

    /// Current service name (if known).
    pub service_name: Option<String>,

    /// Discovered resources.
    resources: Vec<Resource<'py>>,
    /// Created bindings.
    binds: Vec<Bind<'py>>,
    /// Next bind ID.
    next_bind_id: u32,
}

impl<'py> ResourceParseContext<'py> {
    /// Creates a new resource parse context.
    pub fn new(resolver: &'py SchemaParser<'py>, module: &'py ModuleObj<'py>) -> Self {
        ResourceParseContext {
            resolver,
            module_obj: module,
            service_name: None,
            resources: Vec::new(),
            binds: Vec::new(),
            next_bind_id: 1,
        }
    }

    pub fn span(&self, ranged: impl Ranged) -> Span {
        self.module_obj.module.file.id.to_span(ranged)
    }

    /// Alias for `span` for compatibility.
    pub fn range(&self, ranged: impl Ranged) -> Span {
        self.span(ranged)
    }

    /// Checks if a name is imported from a specific module.
    pub fn is_imported_from(&self, name: &str, module_path: &str) -> bool {
        self.module_obj.is_imported_from(name, module_path)
    }

    /// Adds a discovered resource.
    pub fn add_resource(&mut self, resource: Resource<'py>) {
        self.resources.push(resource);
    }

    /// Adds a binding.
    pub fn add_bind(
        &mut self,
        resource: Resource<'py>,
        kind: BindKind,
        span: Span,
        name: Option<String>,
    ) {
        let id = BindId(self.next_bind_id);
        self.next_bind_id += 1;

        let bind = Bind::new(id, span, resource, kind, self.module_obj.module.id, name);
        self.binds.push(bind);
    }

    /// Returns the discovered resources.
    pub fn into_results(self) -> (Vec<Resource<'py>>, Vec<Bind<'py>>) {
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
    /// Both lifetimes must be the same to allow schema parsing.
    pub run: for<'py> fn(&mut ResourceParseContext<'py>) -> ParseResult<()>,
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
    pub fn parsers_for_import(
        &self,
        module_path: &ModulePath,
    ) -> impl Iterator<Item = &'static ResourceParser> + '_ {
        self.interested_for_paths
            .get(module_path.as_str())
            .into_iter()
            .flat_map(|v| v.iter().copied())
    }
}

impl Default for ResourceParserRegistry {
    fn default() -> Self {
        Self::new()
    }
}

/// First pass parser for resource discovery.
pub struct PassOneParser<'py> {
    tr: &'py SchemaParser<'py>,

    /// Parser registry.
    registry: ResourceParserRegistry,
    /// Current bind ID counter.
    next_bind_id: u32,
}

impl<'py> PassOneParser<'py> {
    /// Creates a new pass one parser.
    pub fn new(tr: &'py SchemaParser<'py>) -> Self {
        PassOneParser {
            tr,
            registry: ResourceParserRegistry::new(),
            next_bind_id: 1,
        }
    }

    /// Parses a module for resources.
    pub fn parse_module(
        &'_ mut self,
        module: &'py Module,
        service_name: Option<String>,
    ) -> ParseResult<(Vec<Resource<'py>>, Vec<Bind<'py>>)> {
        let mod_obj = self.tr.inject_module(module);
        let mut ctx = ResourceParseContext::new(&self.tr, mod_obj);

        ctx.service_name = service_name;
        ctx.next_bind_id = self.next_bind_id;

        // Run parsers that are interested in the imported modules
        // let mut parsers_to_run: Vec<&'static ResourceParser> = Vec::new();
        // for module_path in &imported_modules {
        //     for parser in self.registry.parsers_for_import(module_path) {
        //         // Compare by name since ResourceParser doesn't implement PartialEq
        //         let already_added = parsers_to_run.iter().any(|p| p.name == parser.name);
        //         if !already_added {
        //             parsers_to_run.push(parser);
        //         }
        //     }
        // }

        // HACK: run all parsers for now.
        let mut parsers_to_run = self.registry.parsers.clone();

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
            (parser.run)(&mut ctx)?;
        }

        // Update bind ID counter
        self.next_bind_id = ctx.next_bind_id;

        Ok(ctx.into_results())
    }
}
