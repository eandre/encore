use std::{
    collections::{HashMap, HashSet},
    fmt::Debug,
};

use ruff_python_ast as ast;

use crate::ast::loader::{fileset::Span, modpath::ModulePath, Module};

pub trait Object: Debug {
    fn module(&self) -> &Module;
    fn span(&self) -> Span;
    fn name(&self) -> &str;
}

/// The kind of object, with a reference to its corresponding AST node.
#[derive(Debug)]
pub enum ObjectKind<'py> {
    /// A class definition.
    Class(Class<'py>),

    /// A function definition.
    Func(Func<'py>),

    /// A top-level variable assignment.
    Var(Var<'py>),

    /// A module.
    Module(ModuleObj<'py>),
}

#[derive(Debug)]
pub struct Class<'py> {
    pub module: &'py Module,
    pub node: &'py ast::StmtClassDef,
}

#[derive(Debug)]
pub struct Func<'py> {
    pub module: &'py Module,
    pub node: &'py ast::StmtFunctionDef,
}

#[derive(Debug)]
pub struct Var<'py> {
    pub module: &'py Module,
    pub name: String,
    pub value: &'py ast::Expr,
    pub range: AssignmentRange<'py>,
}

impl Object for Class<'_> {
    fn module(&self) -> &Module {
        self.module
    }

    fn span(&self) -> Span {
        self.module.file.id.to_span(self.node.range)
    }

    fn name(&self) -> &str {
        self.node.name.as_str()
    }
}

impl Object for Func<'_> {
    fn module(&self) -> &Module {
        self.module
    }

    fn span(&self) -> Span {
        self.module.file.id.to_span(self.node.range)
    }

    fn name(&self) -> &str {
        self.node.name.as_str()
    }
}

impl Object for Var<'_> {
    fn module(&self) -> &Module {
        self.module
    }

    fn span(&self) -> Span {
        self.module.file.id.to_span(self.range.to_text_range())
    }

    fn name(&self) -> &str {
        &self.name
    }
}

#[derive(Debug)]
pub struct ModuleObj<'py> {
    pub module: &'py Module,

    pub top_level: HashMap<String, ObjectKind<'py>>,

    /// The set of modules imported by this module.
    pub imports: HashSet<ModulePath>,

    /// Maps local name -> module path for imported names.
    /// For `from foo.bar import Baz`, this maps "Baz" -> "foo.bar".
    /// For `from foo.bar import Baz as B`, this maps "B" -> "foo.bar".
    pub import_names: HashMap<String, ModulePath>,
}

impl Object for ModuleObj<'_> {
    fn module(&self) -> &Module {
        self.module
    }

    fn span(&self) -> Span {
        self.module.file.id.to_span(&self.module.ast)
    }

    fn name(&self) -> &str {
        self.module.mod_path.leaf()
    }
}

impl<'py> ModuleObj<'py> {
    pub fn new(module: &'py Module) -> Self {
        let top_level = parse_module_objects(module);
        let imports = collect_imports(module);
        let import_names = collect_import_names(module);
        Self {
            module,
            top_level,
            imports,
            import_names,
        }
    }

    /// Returns the set of modules imported by this module.
    pub fn imports(&self) -> &HashSet<ModulePath> {
        &self.imports
    }

    /// Checks if a name is imported from a specific module.
    pub fn is_imported_from(&self, name: &str, module_path: &str) -> bool {
        self.import_names
            .get(name)
            .map(|m| m.as_str() == module_path)
            .unwrap_or(false)
    }

    /// Returns top-level assignments in this module.
    pub fn assignments(&self) -> Vec<Assignment<'py>> {
        let mut assignments = Vec::new();
        for stmt in &self.module.ast.body {
            if let ast::Stmt::Assign(node) = stmt {
                // Handle simple assignments like `x = ...`
                if node.targets.len() == 1 {
                    if let ast::Expr::Name(name) = &node.targets[0] {
                        assignments.push(Assignment {
                            name: name.id.to_string(),
                            value: &node.value,
                            range: AssignmentRange::Assign(node),
                        });
                    }
                }
            } else if let ast::Stmt::AnnAssign(node) = stmt {
                // Handle annotated assignments like `x: Type = ...`
                if let ast::Expr::Name(name) = &*node.target {
                    if let Some(ref value) = node.value {
                        assignments.push(Assignment {
                            name: name.id.to_string(),
                            value,
                            range: AssignmentRange::AnnAssign(node),
                        });
                    }
                }
            }
        }
        assignments
    }
}

/// A top-level assignment in a module.
#[derive(Debug, Clone)]
pub struct Assignment<'py> {
    /// The name being assigned to.
    pub name: String,
    /// The value expression.
    pub value: &'py ast::Expr,
    /// The range of the assignment statement.
    pub range: AssignmentRange<'py>,
}

/// The range of an assignment, which can be either a regular or annotated assignment.
#[derive(Debug, Clone, Copy)]
pub enum AssignmentRange<'py> {
    Assign(&'py ast::StmtAssign),
    AnnAssign(&'py ast::StmtAnnAssign),
}

impl AssignmentRange<'_> {
    /// Returns the text range of this assignment.
    pub fn to_text_range(&self) -> ruff_text_size::TextRange {
        match self {
            AssignmentRange::Assign(a) => a.range,
            AssignmentRange::AnnAssign(a) => a.range,
        }
    }
}

/// Parses a module and returns all top-level objects (classes, functions, and variables).
fn parse_module_objects<'py>(module: &'py Module) -> HashMap<String, ObjectKind<'py>> {
    let mut objects = HashMap::new();

    for stmt in &module.ast.body {
        match stmt {
            ast::Stmt::ClassDef(node) => {
                let name = node.name.to_string();
                objects.insert(name, ObjectKind::Class(Class { module, node }));
            }
            ast::Stmt::FunctionDef(node) => {
                let name = node.name.to_string();
                objects.insert(name, ObjectKind::Func(Func { module, node }));
            }
            ast::Stmt::Assign(node) => {
                // Handle simple assignments like `x = ...`
                if node.targets.len() == 1 {
                    if let ast::Expr::Name(name_expr) = &node.targets[0] {
                        let name = name_expr.id.to_string();
                        objects.insert(
                            name.clone(),
                            ObjectKind::Var(Var {
                                module,
                                name,
                                value: &node.value,
                                range: AssignmentRange::Assign(node),
                            }),
                        );
                    }
                }
            }
            ast::Stmt::AnnAssign(node) => {
                // Handle annotated assignments like `x: Type = ...`
                if let ast::Expr::Name(name_expr) = &*node.target {
                    if let Some(ref value) = node.value {
                        let name = name_expr.id.to_string();
                        objects.insert(
                            name.clone(),
                            ObjectKind::Var(Var {
                                module,
                                name,
                                value,
                                range: AssignmentRange::AnnAssign(node),
                            }),
                        );
                    }
                }
            }
            _ => {}
        }
    }

    objects
}

/// Collects all modules imported by a module.
fn collect_imports(module: &Module) -> HashSet<ModulePath> {
    let mut imports = HashSet::new();

    for stmt in &module.ast.body {
        match stmt {
            ast::Stmt::Import(node) => {
                for alias in &node.names {
                    if let Some(path) = ModulePath::new(alias.name.as_str()) {
                        imports.insert(path);
                    }
                }
            }
            ast::Stmt::ImportFrom(node) => {
                let module_name = node.module.as_ref().map(|m| m.as_str());
                let level = node.level;
                if let Some(ref pkg_path) = module.pkg_path {
                    if let Some(path) = ModulePath::resolve_import(pkg_path, module_name, level) {
                        imports.insert(path);
                    }
                }
            }
            _ => {}
        }
    }

    imports
}

/// Collects a mapping of imported names to their source module paths.
fn collect_import_names(module: &Module) -> HashMap<String, ModulePath> {
    let mut import_names = HashMap::new();

    for stmt in &module.ast.body {
        match stmt {
            ast::Stmt::Import(node) => {
                // `import foo.bar` or `import foo.bar as baz`
                for alias in &node.names {
                    let local_name =
                        alias
                            .asname
                            .as_ref()
                            .map(|n| n.to_string())
                            .unwrap_or_else(|| {
                                // For `import foo.bar`, the local name is "foo"
                                alias
                                    .name
                                    .as_str()
                                    .split('.')
                                    .next()
                                    .unwrap_or("")
                                    .to_string()
                            });
                    if let Some(mod_path) = ModulePath::new(alias.name.as_str()) {
                        import_names.insert(local_name, mod_path);
                    }
                }
            }
            ast::Stmt::ImportFrom(node) => {
                // `from foo.bar import Baz` or `from foo.bar import Baz as B`
                let module_path = if node.level == 0 {
                    // Absolute import
                    node.module
                        .as_ref()
                        .and_then(|m| ModulePath::new(m.as_str()))
                } else {
                    // Relative import - resolve it
                    let module_name = node.module.as_ref().map(|m| m.as_str());
                    module.pkg_path.as_ref().and_then(|pkg_path| {
                        ModulePath::resolve_import(pkg_path, module_name, node.level)
                    })
                };

                if let Some(mod_path) = module_path {
                    for alias in &node.names {
                        let local_name = alias
                            .asname
                            .as_ref()
                            .map(|n| n.to_string())
                            .unwrap_or_else(|| alias.name.to_string());
                        import_names.insert(local_name, mod_path.clone());
                    }
                }
            }
            _ => {}
        }
    }

    import_names
}
