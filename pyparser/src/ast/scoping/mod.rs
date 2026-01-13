//! Python scope analysis.
//!
//! This module implements Python 3 scoping rules following the LEGB rule:
//! - **L**ocal: Names assigned within a function (def, lambda)
//! - **E**nclosing: Names in local scopes of enclosing functions
//! - **G**lobal: Names assigned at module level or declared `global`
//! - **B**uilt-in: Names in the built-in module
//!
//! Special scoping rules:
//! - Class bodies create a scope but do NOT act as enclosing scopes for methods
//! - Comprehensions (list, dict, set, generator) create their own scope in Python 3
//! - Block statements (if, for, while, try, with) do NOT create scopes

pub mod references;

use std::collections::{HashMap, HashSet};
use std::sync::LazyLock;

use ruff_python_ast as ast;

use crate::ast::loader::modpath::ModulePath;
use crate::ast::visitor::{walk_expr, walk_stmt, Path, PathVisitor};

/// The kind of scope.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ScopeKind {
    /// Module-level (global) scope.
    Module,
    /// Function scope (def or lambda).
    Function,
    /// Class body scope.
    Class,
    /// Comprehension scope (list, dict, set, generator).
    Comprehension,
}

/// Represents how a name is bound in a scope.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BindingKind {
    /// Assigned locally (e.g., `x = 1`).
    Local,
    /// Function parameter.
    Parameter,
    /// Imported name.
    Import,
    /// Loop variable (for loop, comprehension).
    LoopVar,
    /// Exception handler variable.
    ExceptVar,
    /// With statement variable.
    WithVar,
    /// Declared as `global`.
    Global,
    /// Declared as `nonlocal`.
    Nonlocal,
}

/// Information about a binding in a scope.
#[derive(Debug, Clone)]
pub struct Binding {
    /// How the name was bound.
    pub kind: BindingKind,
    /// The scope index where this binding was created.
    pub scope_index: usize,
    /// For imports, the module path the name was imported from.
    pub import_from: Option<ModulePath>,
    /// For imports, the original name in the source module.
    /// E.g., for `from foo import Bar as Baz`, this is "Bar".
    pub original_name: Option<String>,
}

/// A single scope in the scope stack.
#[derive(Debug)]
pub struct Scope {
    /// The kind of scope.
    pub kind: ScopeKind,
    /// Names bound in this scope.
    pub(crate) bindings: HashMap<String, Binding>,
    /// Names declared as `global` in this scope.
    pub(crate) globals: HashSet<String>,
    /// Names declared as `nonlocal` in this scope.
    pub(crate) nonlocals: HashSet<String>,
}

impl Scope {
    /// Creates a new scope of the given kind.
    pub fn new(kind: ScopeKind) -> Self {
        Scope {
            kind,
            bindings: HashMap::new(),
            globals: HashSet::new(),
            nonlocals: HashSet::new(),
        }
    }

    /// Returns whether a name is bound in this scope.
    pub fn has_binding(&self, name: &str) -> bool {
        self.bindings.contains_key(name)
    }

    /// Gets the binding for a name in this scope.
    pub fn get_binding(&self, name: &str) -> Option<&Binding> {
        self.bindings.get(name)
    }

    /// Returns whether a name is declared global in this scope.
    pub fn is_global(&self, name: &str) -> bool {
        self.globals.contains(name)
    }

    /// Returns whether a name is declared nonlocal in this scope.
    pub fn is_nonlocal(&self, name: &str) -> bool {
        self.nonlocals.contains(name)
    }

    /// Returns all bindings in this scope.
    pub fn bindings(&self) -> impl Iterator<Item = (&String, &Binding)> {
        self.bindings.iter()
    }
}

/// The result of looking up a name in scopes.
#[derive(Debug, Clone)]
pub struct NameLookup {
    /// The name that was looked up.
    pub name: String,
    /// The scope where the name was found (index in scope stack).
    pub scope_index: Option<usize>,
    /// The binding information, if found.
    pub binding: Option<Binding>,
    /// Whether this is a builtin name.
    pub is_builtin: bool,
}

/// Static set of built-in names for efficient lookup.
static BUILTINS_SET: LazyLock<HashSet<&'static str>> =
    LazyLock::new(|| BUILTINS.iter().copied().collect());

/// Built-in names in Python 3.
pub const BUILTINS: &[&str] = &[
    // Exceptions
    "ArithmeticError",
    "AssertionError",
    "AttributeError",
    "BaseException",
    "BlockingIOError",
    "BrokenPipeError",
    "BufferError",
    "BytesWarning",
    "ChildProcessError",
    "ConnectionAbortedError",
    "ConnectionError",
    "ConnectionRefusedError",
    "ConnectionResetError",
    "DeprecationWarning",
    "EOFError",
    "EnvironmentError",
    "Exception",
    "FileExistsError",
    "FileNotFoundError",
    "FloatingPointError",
    "FutureWarning",
    "GeneratorExit",
    "IOError",
    "ImportError",
    "ImportWarning",
    "IndentationError",
    "IndexError",
    "InterruptedError",
    "IsADirectoryError",
    "KeyError",
    "KeyboardInterrupt",
    "LookupError",
    "MemoryError",
    "ModuleNotFoundError",
    "NameError",
    "NotADirectoryError",
    "NotImplementedError",
    "OSError",
    "OverflowError",
    "PendingDeprecationWarning",
    "PermissionError",
    "ProcessLookupError",
    "RecursionError",
    "ReferenceError",
    "ResourceWarning",
    "RuntimeError",
    "RuntimeWarning",
    "StopAsyncIteration",
    "StopIteration",
    "SyntaxError",
    "SyntaxWarning",
    "SystemError",
    "SystemExit",
    "TabError",
    "TimeoutError",
    "TypeError",
    "UnboundLocalError",
    "UnicodeDecodeError",
    "UnicodeEncodeError",
    "UnicodeError",
    "UnicodeTranslateError",
    "UnicodeWarning",
    "UserWarning",
    "ValueError",
    "Warning",
    "ZeroDivisionError",
    // Functions
    "abs",
    "aiter",
    "all",
    "any",
    "anext",
    "ascii",
    "bin",
    "bool",
    "breakpoint",
    "bytearray",
    "bytes",
    "callable",
    "chr",
    "classmethod",
    "compile",
    "complex",
    "delattr",
    "dict",
    "dir",
    "divmod",
    "enumerate",
    "eval",
    "exec",
    "filter",
    "float",
    "format",
    "frozenset",
    "getattr",
    "globals",
    "hasattr",
    "hash",
    "help",
    "hex",
    "id",
    "input",
    "int",
    "isinstance",
    "issubclass",
    "iter",
    "len",
    "list",
    "locals",
    "map",
    "max",
    "memoryview",
    "min",
    "next",
    "object",
    "oct",
    "open",
    "ord",
    "pow",
    "print",
    "property",
    "range",
    "repr",
    "reversed",
    "round",
    "set",
    "setattr",
    "slice",
    "sorted",
    "staticmethod",
    "str",
    "sum",
    "super",
    "tuple",
    "type",
    "vars",
    "zip",
    // Constants
    "True",
    "False",
    "None",
    "Ellipsis",
    "NotImplemented",
    "__debug__",
    "__name__",
    "__doc__",
    "__package__",
    "__loader__",
    "__spec__",
    "__annotations__",
    "__builtins__",
    "__file__",
    "__cached__",
];

/// Tracks scopes during AST traversal.
#[derive(Debug)]
pub struct ScopeTracker {
    /// Stack of scopes, from outermost (module) to innermost.
    pub(crate) scopes: Vec<Scope>,
    /// The module path of the current module being analyzed.
    module_path: Option<ModulePath>,
    /// The package path for resolving relative imports.
    /// For `__init__.py` this is the same as module_path.
    /// For regular modules, this is the parent of module_path.
    pkg_path: Option<ModulePath>,
}

impl Default for ScopeTracker {
    fn default() -> Self {
        Self::new(None, false)
    }
}

impl ScopeTracker {
    /// Creates a new scope tracker with an initial module scope.
    ///
    /// # Arguments
    /// * `module_path` - The module path of the current module
    /// * `is_package` - Whether this is a package (`__init__.py`)
    pub fn new(module_path: Option<ModulePath>, is_package: bool) -> Self {
        let pkg_path = module_path.as_ref().and_then(|mp| {
            if is_package {
                Some(mp.clone())
            } else {
                mp.parent()
            }
        });
        ScopeTracker {
            scopes: vec![Scope::new(ScopeKind::Module)],
            module_path,
            pkg_path,
        }
    }

    /// Returns the module path of the current module.
    pub fn module_path(&self) -> Option<&ModulePath> {
        self.module_path.as_ref()
    }

    /// Returns the package path for resolving relative imports.
    pub fn pkg_path(&self) -> Option<&ModulePath> {
        self.pkg_path.as_ref()
    }

    /// Resolves a relative import to an absolute module path.
    ///
    /// # Arguments
    /// * `module` - The module name (e.g., "foo" in `from .foo import bar`)
    /// * `level` - The relative import level (1 for `.`, 2 for `..`, etc.)
    pub fn resolve_import(&self, module: Option<&str>, level: u32) -> Option<ModulePath> {
        ModulePath::resolve_import(self.pkg_path.as_ref()?, module, level)
    }

    /// Returns the current scope index.
    pub fn current_scope_index(&self) -> usize {
        self.scopes.len() - 1
    }

    /// Returns the current scope.
    pub fn current_scope(&self) -> &Scope {
        self.scopes.last().expect("scope stack should never be empty")
    }

    /// Returns a mutable reference to the current scope.
    pub fn current_scope_mut(&mut self) -> &mut Scope {
        self.scopes.last_mut().expect("scope stack should never be empty")
    }

    /// Returns the scope at the given index.
    pub fn scope(&self, index: usize) -> Option<&Scope> {
        self.scopes.get(index)
    }

    /// Returns an iterator over all scopes from innermost to outermost.
    pub fn scopes(&self) -> impl Iterator<Item = (usize, &Scope)> {
        self.scopes.iter().enumerate().rev()
    }

    /// Pushes a new scope onto the stack.
    pub fn push_scope(&mut self, kind: ScopeKind) {
        self.scopes.push(Scope::new(kind));
    }

    /// Pops the current scope from the stack.
    pub fn pop_scope(&mut self) -> Option<Scope> {
        // Never pop the module scope
        if self.scopes.len() > 1 {
            self.scopes.pop()
        } else {
            None
        }
    }

    /// Declares a name as `global` in the current scope.
    pub fn declare_global(&mut self, name: &str) {
        self.current_scope_mut().globals.insert(name.to_string());
    }

    /// Declares a name as `nonlocal` in the current scope.
    pub fn declare_nonlocal(&mut self, name: &str) {
        self.current_scope_mut().nonlocals.insert(name.to_string());
    }

    /// Binds a name in the appropriate scope based on global/nonlocal declarations.
    pub fn bind_name(&mut self, name: &str, kind: BindingKind) {
        self.bind_name_with_import(name, kind, None, None);
    }

    /// Binds an imported name with its source module path and original name.
    ///
    /// # Arguments
    /// * `local_name` - The name bound locally (e.g., "Baz" in `import Foo as Baz`)
    /// * `from_module` - The module the name was imported from
    /// * `original_name` - The original name in the source module (e.g., "Foo")
    pub fn bind_import(
        &mut self,
        local_name: &str,
        from_module: ModulePath,
        original_name: &str,
    ) {
        self.bind_name_with_import(
            local_name,
            BindingKind::Import,
            Some(from_module),
            Some(original_name.to_string()),
        );
    }

    /// Binds a name with an optional import source.
    fn bind_name_with_import(
        &mut self,
        name: &str,
        kind: BindingKind,
        import_from: Option<ModulePath>,
        original_name: Option<String>,
    ) {
        let current_idx = self.current_scope_index();
        let current = self.current_scope();

        // Check for global declaration
        if current.is_global(name) {
            // Bind in module (global) scope
            let binding = Binding {
                kind,
                scope_index: 0,
                import_from,
                original_name,
            };
            self.scopes[0].bindings.insert(name.to_string(), binding);
            return;
        }

        // Check for nonlocal declaration
        if current.is_nonlocal(name) {
            // Find the enclosing function scope that has this name
            if let Some(scope_idx) = self.find_enclosing_binding(name) {
                let binding = Binding {
                    kind,
                    scope_index: scope_idx,
                    import_from,
                    original_name,
                };
                self.scopes[scope_idx]
                    .bindings
                    .insert(name.to_string(), binding);
                return;
            }
        }

        // Bind in current scope
        let binding = Binding {
            kind,
            scope_index: current_idx,
            import_from,
            original_name,
        };
        self.current_scope_mut()
            .bindings
            .insert(name.to_string(), binding);
    }

    /// Looks up a name following Python's LEGB rule.
    pub fn lookup(&self, name: &str) -> NameLookup {
        // Check current scope for global/nonlocal declarations
        let current = self.current_scope();

        // If declared global, look only in global scope
        if current.is_global(name) {
            if let Some(binding) = self.scopes[0].get_binding(name) {
                return NameLookup {
                    name: name.to_string(),
                    scope_index: Some(0),
                    binding: Some(binding.clone()),
                    is_builtin: false,
                };
            }
            // Not found in global scope, but declared global
            return NameLookup {
                name: name.to_string(),
                scope_index: None,
                binding: None,
                is_builtin: false,
            };
        }

        // If declared nonlocal, look in enclosing function scopes
        if current.is_nonlocal(name) {
            for (idx, scope) in self.scopes().skip(1) {
                // Skip class scopes for nonlocal lookup
                if scope.kind == ScopeKind::Class {
                    continue;
                }
                if let Some(binding) = scope.get_binding(name) {
                    return NameLookup {
                        name: name.to_string(),
                        scope_index: Some(idx),
                        binding: Some(binding.clone()),
                        is_builtin: false,
                    };
                }
            }
        }

        // Normal LEGB lookup
        // L: Local scope
        if let Some(binding) = current.get_binding(name) {
            return NameLookup {
                name: name.to_string(),
                scope_index: Some(self.current_scope_index()),
                binding: Some(binding.clone()),
                is_builtin: false,
            };
        }

        // E: Enclosing function scopes (skip class scopes!)
        for (idx, scope) in self.scopes().skip(1) {
            // Class scopes don't act as enclosing scopes for name lookup
            if scope.kind == ScopeKind::Class {
                continue;
            }
            if let Some(binding) = scope.get_binding(name) {
                return NameLookup {
                    name: name.to_string(),
                    scope_index: Some(idx),
                    binding: Some(binding.clone()),
                    is_builtin: false,
                };
            }
        }

        // G: Global (module) scope - already checked in enclosing loop if not skipped

        // B: Built-in scope
        if BUILTINS_SET.contains(name) {
            return NameLookup {
                name: name.to_string(),
                scope_index: None,
                binding: None,
                is_builtin: true,
            };
        }

        // Not found
        NameLookup {
            name: name.to_string(),
            scope_index: None,
            binding: None,
            is_builtin: false,
        }
    }

    /// Finds the enclosing scope that has a binding for the given name.
    /// Used for nonlocal resolution. Skips class scopes.
    fn find_enclosing_binding(&self, name: &str) -> Option<usize> {
        for (idx, scope) in self.scopes().skip(1) {
            if scope.kind == ScopeKind::Class {
                continue;
            }
            if scope.has_binding(name) {
                return Some(idx);
            }
        }
        None
    }

    /// Returns true if we're currently in a function scope (including nested).
    pub fn in_function(&self) -> bool {
        self.scopes
            .iter()
            .any(|s| s.kind == ScopeKind::Function)
    }

    /// Returns true if we're currently in a class scope.
    pub fn in_class(&self) -> bool {
        self.scopes.iter().any(|s| s.kind == ScopeKind::Class)
    }

    /// Returns the nearest enclosing function scope index, if any.
    pub fn enclosing_function_scope(&self) -> Option<usize> {
        self.scopes()
            .find(|(_, s)| s.kind == ScopeKind::Function)
            .map(|(idx, _)| idx)
    }
}

/// A visitor that tracks scopes while traversing the AST.
///
/// This visitor maintains a `ScopeTracker` and properly enters/exits scopes
/// as it traverses function definitions, class definitions, and comprehensions.
pub struct ScopeVisitor<'a> {
    /// The scope tracker.
    pub tracker: ScopeTracker,
    /// Marker for the AST lifetime.
    _marker: std::marker::PhantomData<&'a ()>,
}

impl<'a> Default for ScopeVisitor<'a> {
    fn default() -> Self {
        Self::new(None, false)
    }
}

impl<'a> ScopeVisitor<'a> {
    /// Creates a new scope visitor.
    ///
    /// # Arguments
    /// * `module_path` - The module path of the current module
    /// * `is_package` - Whether this is a package (`__init__.py`)
    pub fn new(module_path: Option<ModulePath>, is_package: bool) -> Self {
        ScopeVisitor {
            tracker: ScopeTracker::new(module_path, is_package),
            _marker: std::marker::PhantomData,
        }
    }

    /// Returns a reference to the scope tracker.
    pub fn tracker(&self) -> &ScopeTracker {
        &self.tracker
    }

    /// Binds names from an assignment target expression.
    fn bind_target(&mut self, target: &ast::Expr, kind: BindingKind) {
        match target {
            ast::Expr::Name(name) => {
                self.tracker.bind_name(name.id.as_str(), kind);
            }
            ast::Expr::Tuple(tuple) => {
                for elt in &tuple.elts {
                    self.bind_target(elt, kind);
                }
            }
            ast::Expr::List(list) => {
                for elt in &list.elts {
                    self.bind_target(elt, kind);
                }
            }
            ast::Expr::Starred(starred) => {
                self.bind_target(&starred.value, kind);
            }
            // Attribute and subscript assignments don't create local bindings
            ast::Expr::Attribute(_) | ast::Expr::Subscript(_) => {}
            _ => {}
        }
    }

    /// Binds function parameters.
    fn bind_parameters(&mut self, params: &ast::Parameters) {
        // Positional-only parameters
        for param in &params.posonlyargs {
            self.tracker
                .bind_name(param.parameter.name.as_str(), BindingKind::Parameter);
        }
        // Regular parameters
        for param in &params.args {
            self.tracker
                .bind_name(param.parameter.name.as_str(), BindingKind::Parameter);
        }
        // Keyword-only parameters
        for param in &params.kwonlyargs {
            self.tracker
                .bind_name(param.parameter.name.as_str(), BindingKind::Parameter);
        }
        // *args
        if let Some(vararg) = &params.vararg {
            self.tracker
                .bind_name(vararg.name.as_str(), BindingKind::Parameter);
        }
        // **kwargs
        if let Some(kwarg) = &params.kwarg {
            self.tracker
                .bind_name(kwarg.name.as_str(), BindingKind::Parameter);
        }
    }
}

impl<'a> PathVisitor<'a> for ScopeVisitor<'a> {
    fn visit_stmt(&mut self, stmt: &'a ast::Stmt, path: &mut Path<'a>) {
        match stmt {
            // Function definition creates a new function scope
            ast::Stmt::FunctionDef(func) => {
                // The function name is bound in the *current* scope
                self.tracker
                    .bind_name(func.name.as_str(), BindingKind::Local);

                // Enter function scope for the body
                self.tracker.push_scope(ScopeKind::Function);

                // Bind parameters in the new scope
                self.bind_parameters(&func.parameters);

                // Continue walking (will visit decorators, body, etc.)
                walk_stmt(self, stmt, path);

                // Exit function scope
                self.tracker.pop_scope();
                return; // Don't call walk_stmt again
            }

            // Class definition creates a class scope
            ast::Stmt::ClassDef(class) => {
                // The class name is bound in the current scope
                self.tracker
                    .bind_name(class.name.as_str(), BindingKind::Local);

                // Enter class scope for the body
                self.tracker.push_scope(ScopeKind::Class);

                // Continue walking
                walk_stmt(self, stmt, path);

                // Exit class scope
                self.tracker.pop_scope();
                return;
            }

            // Global declaration
            ast::Stmt::Global(global) => {
                for name in &global.names {
                    self.tracker.declare_global(name.as_str());
                }
            }

            // Nonlocal declaration
            ast::Stmt::Nonlocal(nonlocal) => {
                for name in &nonlocal.names {
                    self.tracker.declare_nonlocal(name.as_str());
                }
            }

            // Assignment binds names
            ast::Stmt::Assign(assign) => {
                for target in &assign.targets {
                    self.bind_target(target, BindingKind::Local);
                }
            }

            // Annotated assignment binds names
            ast::Stmt::AnnAssign(ann_assign) => {
                // Only binds if there's a value, or if it's a simple name at module/class level
                if ann_assign.value.is_some() || ann_assign.simple {
                    self.bind_target(&ann_assign.target, BindingKind::Local);
                }
            }

            // Augmented assignment binds names (x += 1 implies x exists)
            ast::Stmt::AugAssign(aug_assign) => {
                self.bind_target(&aug_assign.target, BindingKind::Local);
            }

            // For loop binds the target variable
            ast::Stmt::For(for_stmt) => {
                self.bind_target(&for_stmt.target, BindingKind::LoopVar);
            }

            // Import statements bind names
            ast::Stmt::Import(import) => {
                for alias in &import.names {
                    let original_name = alias.name.as_str();
                    let local_name = alias
                        .asname
                        .as_ref()
                        .map(|n| n.as_str())
                        .unwrap_or_else(|| {
                            // For `import foo.bar`, only `foo` is bound
                            original_name.split('.').next().unwrap_or("")
                        });
                    // The module path is the full import path
                    if let Some(module_path) = ModulePath::new(original_name) {
                        self.tracker
                            .bind_import(local_name, module_path, original_name);
                    } else {
                        self.tracker.bind_name(local_name, BindingKind::Import);
                    }
                }
            }

            ast::Stmt::ImportFrom(import) => {
                // Resolve the module path (handles relative imports)
                let module_name = import.module.as_ref().map(|m| m.as_str());
                let level = import.level;
                let resolved_module = if level == 0 {
                    // Absolute import
                    module_name.and_then(ModulePath::new)
                } else {
                    // Relative import
                    self.tracker.resolve_import(module_name, level)
                };

                for alias in &import.names {
                    let original_name = alias.name.as_str();
                    let local_name = alias
                        .asname
                        .as_ref()
                        .map(|n| n.as_str())
                        .unwrap_or(original_name);
                    // Skip star imports for binding (they import all names)
                    if local_name != "*" {
                        if let Some(ref module_path) = resolved_module {
                            self.tracker
                                .bind_import(local_name, module_path.clone(), original_name);
                        } else {
                            self.tracker.bind_name(local_name, BindingKind::Import);
                        }
                    }
                }
            }

            // With statement binds optional vars
            ast::Stmt::With(with_stmt) => {
                for item in &with_stmt.items {
                    if let Some(vars) = &item.optional_vars {
                        self.bind_target(vars, BindingKind::WithVar);
                    }
                }
            }

            // Try/except binds exception names
            ast::Stmt::Try(try_stmt) => {
                for handler in &try_stmt.handlers {
                    let ast::ExceptHandler::ExceptHandler(h) = handler;
                    if let Some(name) = &h.name {
                        self.tracker.bind_name(name.as_str(), BindingKind::ExceptVar);
                    }
                }
            }

            // Named expression (walrus operator) binds in enclosing non-comprehension scope
            // This is handled in visit_expr

            _ => {}
        }

        // Default: continue walking
        walk_stmt(self, stmt, path);
    }

    fn visit_expr(&mut self, expr: &'a ast::Expr, path: &mut Path<'a>) {
        match expr {
            // Lambda creates a function scope
            ast::Expr::Lambda(lambda) => {
                // Enter function scope
                self.tracker.push_scope(ScopeKind::Function);

                // Bind parameters
                if let Some(params) = &lambda.parameters {
                    self.bind_parameters(params);
                }

                // Continue walking
                walk_expr(self, expr, path);

                // Exit function scope
                self.tracker.pop_scope();
                return;
            }

            // List/Set/Dict comprehensions and generators create their own scope in Python 3
            ast::Expr::ListComp(comp) => {
                self.tracker.push_scope(ScopeKind::Comprehension);
                // Bind the iteration variables
                for generator in &comp.generators {
                    self.bind_target(&generator.target, BindingKind::LoopVar);
                }
                walk_expr(self, expr, path);
                self.tracker.pop_scope();
                return;
            }

            ast::Expr::SetComp(comp) => {
                self.tracker.push_scope(ScopeKind::Comprehension);
                for generator in &comp.generators {
                    self.bind_target(&generator.target, BindingKind::LoopVar);
                }
                walk_expr(self, expr, path);
                self.tracker.pop_scope();
                return;
            }

            ast::Expr::DictComp(comp) => {
                self.tracker.push_scope(ScopeKind::Comprehension);
                for generator in &comp.generators {
                    self.bind_target(&generator.target, BindingKind::LoopVar);
                }
                walk_expr(self, expr, path);
                self.tracker.pop_scope();
                return;
            }

            ast::Expr::Generator(gen) => {
                self.tracker.push_scope(ScopeKind::Comprehension);
                for generator in &gen.generators {
                    self.bind_target(&generator.target, BindingKind::LoopVar);
                }
                walk_expr(self, expr, path);
                self.tracker.pop_scope();
                return;
            }

            // Named expression (walrus operator := ) binds in the enclosing non-comprehension scope
            ast::Expr::Named(named) => {
                // Find the target scope (skip comprehension scopes)
                let target_scope_idx = self
                    .tracker
                    .scopes()
                    .find(|(_, s)| s.kind != ScopeKind::Comprehension)
                    .map(|(idx, _)| idx)
                    .unwrap_or(0);

                if let ast::Expr::Name(name) = &*named.target {
                    let binding = Binding {
                        kind: BindingKind::Local,
                        scope_index: target_scope_idx,
                        import_from: None,
                        original_name: None,
                    };
                    self.tracker.scopes[target_scope_idx]
                        .bindings
                        .insert(name.id.to_string(), binding);
                }
            }

            _ => {}
        }

        // Default: continue walking
        walk_expr(self, expr, path);
    }
}

#[cfg(test)]
mod tests;
