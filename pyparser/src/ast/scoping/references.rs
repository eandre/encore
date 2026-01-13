//! Reference tracking with scope awareness.
//!
//! This module provides utilities for finding references to names imported
//! from specific modules, using scope tracking to ensure accuracy.
//!
//! Unlike simple import scanning, this approach:
//! - Handles shadowing correctly (reassigned names won't match)
//! - Handles aliased imports correctly (tracks original module)
//! - Respects Python's scoping rules (LEGB)

use std::collections::HashMap;

use ruff_python_ast as ast;
use ruff_text_size::Ranged;

use super::{BindingKind, ScopeKind, ScopeVisitor};
use crate::ast::loader::errors::{ParseError, ParseResult};
use crate::ast::loader::fileset::{FileId, Span};
use crate::ast::loader::modpath::ModulePath;
use crate::ast::visitor::{walk_expr, walk_stmt, Path, PathVisitor};

/// Tracks names from specific modules that we're interested in finding references to.
///
/// Used to efficiently determine which identifiers in a module
/// might be references to tracked resources.
pub struct TrackedNames<'a>(HashMap<&'a str, Vec<&'a str>>);

impl<'a> TrackedNames<'a> {
    /// Creates a new TrackedNames from a slice of (module, name) pairs.
    ///
    /// # Example
    /// ```ignore
    /// let tracked = TrackedNames::new(&[
    ///     ("encoredev.pubsub", "Topic"),
    ///     ("encoredev.pubsub", "Subscription"),
    ///     ("encore.sqldb", "Database"),
    /// ]);
    /// ```
    pub fn new(names: &'a [(&'a str, &'a str)]) -> Self {
        let mut modules = HashMap::new();
        for &(module, name) in names {
            modules.entry(module).or_insert_with(Vec::new).push(name);
        }
        Self(modules)
    }

    /// Returns the names tracked for a given module.
    pub fn get(&self, module: &str) -> Option<&[&str]> {
        self.0.get(module).map(|v| &v[..])
    }

    /// Returns an iterator over all (module, name) pairs being tracked.
    pub fn iter(&self) -> impl Iterator<Item = (&str, &str)> + '_ {
        self.0
            .iter()
            .flat_map(|(&module, names)| names.iter().map(move |&name| (module, name)))
    }

    /// Checks if a name from a given module is being tracked.
    pub fn is_tracked(&self, module: &str, name: &str) -> bool {
        self.0
            .get(module)
            .map(|names| names.contains(&name))
            .unwrap_or(false)
    }
}

/// Context provided to ReferenceParser when parsing a reference.
///
/// This context is provided for any reference to a tracked import,
/// whether it's a call expression, decorator, or other usage.
///
/// The lifetime `'ast` represents the lifetime of the AST nodes being referenced.
/// The lifetime `'path` represents the lifetime of the path reference (may be shorter).
pub struct ReferenceContext<'path, 'ast> {
    /// The expression referencing the tracked import.
    /// This could be a Name, Call, Subscript (for generics), etc.
    pub expr: &'ast ast::Expr,
    /// The module path the name was imported from.
    pub import_module: ModulePath,
    /// The name of the variable being assigned to, if any.
    pub bind_name: Option<String>,
    /// The AST path to the current expression.
    pub path: &'path Path<'ast>,
    /// File ID for error reporting.
    file_id: FileId,
}

impl<'path, 'ast> ReferenceContext<'path, 'ast> {
    /// Returns the referenced name (the import name being used).
    pub fn name(&self) -> Option<&str> {
        match self.expr {
            ast::Expr::Name(name) => Some(name.id.as_str()),
            ast::Expr::Call(call) => match &*call.func {
                ast::Expr::Name(name) => Some(name.id.as_str()),
                ast::Expr::Subscript(sub) => {
                    if let ast::Expr::Name(name) = &*sub.value {
                        Some(name.id.as_str())
                    } else {
                        None
                    }
                }
                _ => None,
            },
            ast::Expr::Subscript(sub) => {
                if let ast::Expr::Name(name) = &*sub.value {
                    Some(name.id.as_str())
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    /// Returns the expression as a call, if it is one.
    pub fn as_call(&self) -> Option<&'ast ast::ExprCall> {
        match self.expr {
            ast::Expr::Call(call) => Some(call),
            _ => None,
        }
    }

    /// Gets the first positional argument as a string (if this is a call).
    pub fn first_string_arg(&self) -> Option<String> {
        self.as_call()
            .and_then(|call| call.arguments.args.first())
            .and_then(|e| extract_string(e))
    }

    /// Gets a positional argument by index (if this is a call).
    pub fn arg(&self, idx: usize) -> Option<&'ast ast::Expr> {
        self.as_call()
            .and_then(|call| call.arguments.args.get(idx))
    }

    /// Gets a keyword argument by name (if this is a call).
    pub fn kwarg(&self, name: &str) -> Option<&'ast ast::Expr> {
        let call = self.as_call()?;
        for kw in &call.arguments.keywords {
            if let Some(arg) = &kw.arg {
                if arg.as_str() == name {
                    return Some(&kw.value);
                }
            }
        }
        None
    }

    /// Gets the text range of the expression.
    pub fn range(&self) -> ruff_text_size::TextRange {
        self.expr.range()
    }

    /// Creates a span for the given range.
    pub fn span(&self, range: ruff_text_size::TextRange) -> Span {
        Span::new(self.file_id, range.start().into(), range.end().into())
    }

    /// Creates a parse error with the expression's span.
    pub fn error(&self, message: impl Into<String>) -> ParseError {
        ParseError::Parse {
            span: self.span(self.expr.range()),
            message: message.into(),
        }
    }

    /// Creates a parse error for a specific range.
    pub fn error_at(&self, range: ruff_text_size::TextRange, message: impl Into<String>) -> ParseError {
        ParseError::Parse {
            span: self.span(range),
            message: message.into(),
        }
    }

    /// Returns the file ID for this context.
    pub fn file_id(&self) -> FileId {
        self.file_id
    }

    /// Returns the decorated function if this reference is used as a decorator.
    ///
    /// This checks if the current expression (or its parent call) is in the
    /// decorator list of a function definition. Returns the function if so.
    ///
    /// Handles both:
    /// - `@decorator` - bare decorator
    /// - `@decorator(...)` - decorator with arguments
    pub fn as_decorator(&self) -> Option<&'ast ast::StmtFunctionDef> {
        // Walk up the path to find if we're inside a decorator context
        for node in self.path.iter_ancestors() {
            if let Some(stmt) = node.as_stmt() {
                if let ast::Stmt::FunctionDef(func) = stmt {
                    // Check if expr is one of the decorators
                    for decorator in &func.decorator_list {
                        let dec_expr = &decorator.expression;
                        // Decorator can be the expr itself, or we might be inside it
                        if std::ptr::eq(dec_expr, self.expr) {
                            return Some(func);
                        }
                        // For @decorator(...), the call contains our tracked name
                        if let ast::Expr::Call(call) = dec_expr {
                            if std::ptr::eq(call.func.as_ref(), self.expr)
                                || std::ptr::eq(dec_expr, self.expr)
                            {
                                return Some(func);
                            }
                        }
                    }
                }
            }
        }
        None
    }
}

/// A parsed config dictionary with string keys.
///
/// This is a thin wrapper around dict expressions that provides
/// convenient access to literal values using `pylitparser::LitParser`.
#[derive(Debug, Clone, Default)]
pub struct LiteralDict<'a> {
    entries: Vec<(String, &'a ast::Expr)>,
}

impl<'a> LiteralDict<'a> {
    /// Parses a dict expression into a LiteralDict.
    ///
    /// Only includes entries where the key is a string literal.
    /// Non-string keys are silently ignored.
    pub fn parse(expr: &'a ast::Expr) -> Option<Self> {
        let ast::Expr::Dict(dict) = expr else {
            return None;
        };

        let mut entries = Vec::new();
        for (key, value) in dict.iter_keys().zip(dict.iter_values()) {
            // Only process string keys
            let Some(key_expr) = key else { continue };
            let Some(key_str) = extract_string(key_expr) else {
                continue;
            };
            entries.push((key_str, value));
        }

        Some(LiteralDict { entries })
    }

    /// Gets a value by key and parses it using the `LitParser` trait.
    pub fn get<T: pylitparser::LitParser<'a>>(&self, key: &str) -> Option<T> {
        self.entries
            .iter()
            .find(|(k, _)| k == key)
            .and_then(|(_, v)| T::parse_lit(v).ok())
    }

    /// Gets the raw expression for a key.
    pub fn get_expr(&self, key: &str) -> Option<&'a ast::Expr> {
        self.entries
            .iter()
            .find(|(k, _)| k == key)
            .map(|(_, v)| *v)
    }

    /// Returns an iterator over all entries with their raw expressions.
    pub fn iter(&self) -> impl Iterator<Item = (&str, &'a ast::Expr)> {
        self.entries.iter().map(|(k, v)| (k.as_str(), *v))
    }
}

/// Extracts a string value from an expression.
pub fn extract_string(expr: &ast::Expr) -> Option<String> {
    match expr {
        ast::Expr::StringLiteral(lit) => Some(lit.value.to_string()),
        _ => None,
    }
}

/// Extracts a boolean value from an expression.
pub fn extract_bool(expr: &ast::Expr) -> Option<bool> {
    match expr {
        ast::Expr::BooleanLiteral(lit) => Some(lit.value),
        _ => None,
    }
}

/// Extracts an integer value from an expression.
pub fn extract_int(expr: &ast::Expr) -> Option<i64> {
    match expr {
        ast::Expr::NumberLiteral(lit) => match &lit.value {
            ast::Number::Int(i) => i.as_i64(),
            _ => None,
        },
        _ => None,
    }
}

/// Trait for parsing resource references from expressions.
///
/// Implement this trait for types that can be constructed from AST patterns,
/// such as resource definitions or usages.
///
/// The lifetime parameter `'ast` allows implementations to hold references
/// to the AST nodes being parsed.
pub trait ReferenceParser<'ast>
where
    Self: Sized,
{
    /// Attempts to parse a resource reference from the reference context.
    ///
    /// Returns:
    /// - `Ok(Some(resource))` if this is a valid resource reference
    /// - `Ok(None)` if this is not a resource reference (continue searching)
    /// - `Err(ParseError)` if this looks like a resource reference but is malformed
    fn parse_resource_reference(ctx: &ReferenceContext<'_, 'ast>) -> ParseResult<Option<Self>>;
}

/// Iterates over references to tracked names in a module's AST.
///
/// This function uses scope tracking to accurately identify references to
/// names imported from specific modules. It properly handles:
/// - Shadowing (local assignments that hide imports)
/// - Aliased imports (`from foo import Bar as Baz`)
/// - Nested scopes (names in inner scopes that shadow outer ones)
///
/// # Arguments
/// * `ast` - The parsed module AST
/// * `names` - The set of (module, name) pairs to look for
/// * `module_path` - The module path of the module being analyzed (for relative imports)
/// * `is_package` - Whether this module is a package (`__init__.py`)
/// * `file_id` - The file ID for error reporting
///
/// # Returns
/// A vector of results, where each result is either a successfully parsed reference
/// or a ParseError describing why parsing failed.
pub fn iter_references<'ast, 'names, R: ReferenceParser<'ast>>(
    ast: &'ast ast::ModModule,
    names: &'names TrackedNames<'names>,
    module_path: Option<ModulePath>,
    is_package: bool,
    file_id: FileId,
) -> Vec<ParseResult<R>> {
    let mut visitor = ReferenceVisitor::<'ast, 'names, R>::new(names, module_path, is_package, file_id);

    let mut path = Path::new();
    visitor.visit_module(ast, &mut path);

    visitor.results
}

/// Visitor that collects resource references while tracking scopes.
///
/// This combines `ScopeVisitor` functionality with reference tracking,
/// ensuring that name lookups respect Python's scoping rules.
struct ReferenceVisitor<'ast, 'names, R> {
    /// The underlying scope visitor for tracking bindings.
    scope_visitor: ScopeVisitor<'ast>,
    /// The names we're looking for references to.
    tracked_names: &'names TrackedNames<'names>,
    /// File ID for error reporting.
    file_id: FileId,
    /// Collected results.
    results: Vec<ParseResult<R>>,
}

impl<'ast, 'names, R> ReferenceVisitor<'ast, 'names, R> {
    fn new(
        tracked_names: &'names TrackedNames<'names>,
        module_path: Option<ModulePath>,
        is_package: bool,
        file_id: FileId,
    ) -> Self {
        Self {
            scope_visitor: ScopeVisitor::new(module_path, is_package),
            tracked_names,
            file_id,
            results: Vec::new(),
        }
    }

    /// Checks if an expression is a reference to a tracked import.
    ///
    /// Returns the module path the name was imported from if it's a tracked reference.
    fn check_tracked_reference(&self, expr: &ast::Expr) -> Option<ModulePath> {
        // Extract the name from the expression - can be Name or Subscript (for generics)
        let name = match expr {
            ast::Expr::Name(name) => name,
            ast::Expr::Subscript(sub) => {
                // Handle generic types like Topic[str]
                if let ast::Expr::Name(name) = &*sub.value {
                    name
                } else {
                    return None;
                }
            }
            _ => return None,
        };

        // Look up the name in the current scope
        let lookup = self.scope_visitor.tracker().lookup(name.id.as_str());

        // Must have a binding with an import source
        let binding = lookup.binding.as_ref()?;
        if binding.kind != BindingKind::Import {
            return None;
        }

        let import_from = binding.import_from.as_ref()?;

        // Get the original imported name (for aliased imports like `from foo import Bar as Baz`)
        // If no original_name, fall back to the local name
        let original_name = binding
            .original_name
            .as_deref()
            .unwrap_or_else(|| name.id.as_str());

        // Check if this import is from a tracked module and the original name is tracked
        if let Some(tracked) = self.tracked_names.get(import_from.as_str()) {
            if tracked.iter().any(|&tracked_name| tracked_name == original_name) {
                return Some(import_from.clone());
            }
        }

        None
    }

    /// Extracts the bind name from an assignment context.
    fn extract_bind_name(&self, path: &Path<'_>) -> Option<String> {
        for node in path.iter_ancestors() {
            if let Some(stmt) = node.as_stmt() {
                match stmt {
                    ast::Stmt::Assign(assign) => {
                        if assign.targets.len() == 1 {
                            if let ast::Expr::Name(name) = &assign.targets[0] {
                                return Some(name.id.to_string());
                            }
                        }
                    }
                    ast::Stmt::AnnAssign(ann_assign) => {
                        if let ast::Expr::Name(name) = &*ann_assign.target {
                            return Some(name.id.to_string());
                        }
                    }
                    _ => {}
                }
            }
        }
        None
    }

    /// Binds names from an assignment target expression.
    fn bind_target(&mut self, target: &ast::Expr, kind: super::BindingKind) {
        match target {
            ast::Expr::Name(name) => {
                self.scope_visitor.tracker.bind_name(name.id.as_str(), kind);
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
            self.scope_visitor
                .tracker
                .bind_name(param.parameter.name.as_str(), super::BindingKind::Parameter);
        }
        // Regular parameters
        for param in &params.args {
            self.scope_visitor
                .tracker
                .bind_name(param.parameter.name.as_str(), super::BindingKind::Parameter);
        }
        // Keyword-only parameters
        for param in &params.kwonlyargs {
            self.scope_visitor
                .tracker
                .bind_name(param.parameter.name.as_str(), super::BindingKind::Parameter);
        }
        // *args
        if let Some(vararg) = &params.vararg {
            self.scope_visitor
                .tracker
                .bind_name(vararg.name.as_str(), super::BindingKind::Parameter);
        }
        // **kwargs
        if let Some(kwarg) = &params.kwarg {
            self.scope_visitor
                .tracker
                .bind_name(kwarg.name.as_str(), super::BindingKind::Parameter);
        }
    }
}

impl<'ast, 'names, R: ReferenceParser<'ast>> PathVisitor<'ast> for ReferenceVisitor<'ast, 'names, R> {
    fn visit_stmt(&mut self, stmt: &'ast ast::Stmt, path: &mut Path<'ast>) {
        match stmt {
            // Function definition creates a new function scope
            ast::Stmt::FunctionDef(func) => {
                // The function name is bound in the *current* scope
                self.scope_visitor
                    .tracker
                    .bind_name(func.name.as_str(), super::BindingKind::Local);

                // Enter function scope for the body
                self.scope_visitor.tracker.push_scope(ScopeKind::Function);

                // Bind parameters in the new scope
                self.bind_parameters(&func.parameters);

                // Continue walking (will visit decorators, body, etc.)
                walk_stmt(self, stmt, path);

                // Exit function scope
                self.scope_visitor.tracker.pop_scope();
                return;
            }

            // Class definition creates a class scope
            ast::Stmt::ClassDef(class) => {
                // The class name is bound in the current scope
                self.scope_visitor
                    .tracker
                    .bind_name(class.name.as_str(), super::BindingKind::Local);

                // Enter class scope for the body
                self.scope_visitor.tracker.push_scope(ScopeKind::Class);

                // Continue walking
                walk_stmt(self, stmt, path);

                // Exit class scope
                self.scope_visitor.tracker.pop_scope();
                return;
            }

            // Global declaration
            ast::Stmt::Global(global) => {
                for name in &global.names {
                    self.scope_visitor.tracker.declare_global(name.as_str());
                }
            }

            // Nonlocal declaration
            ast::Stmt::Nonlocal(nonlocal) => {
                for name in &nonlocal.names {
                    self.scope_visitor.tracker.declare_nonlocal(name.as_str());
                }
            }

            // Assignment binds names
            ast::Stmt::Assign(assign) => {
                for target in &assign.targets {
                    self.bind_target(target, super::BindingKind::Local);
                }
            }

            // Annotated assignment binds names
            ast::Stmt::AnnAssign(ann_assign) => {
                if ann_assign.value.is_some() || ann_assign.simple {
                    self.bind_target(&ann_assign.target, super::BindingKind::Local);
                }
            }

            // Augmented assignment binds names
            ast::Stmt::AugAssign(aug_assign) => {
                self.bind_target(&aug_assign.target, super::BindingKind::Local);
            }

            // For loop binds the target variable
            ast::Stmt::For(for_stmt) => {
                self.bind_target(&for_stmt.target, super::BindingKind::LoopVar);
            }

            // Import statements bind names
            ast::Stmt::Import(import) => {
                for alias in &import.names {
                    let original_name = alias.name.as_str();
                    let local_name = alias
                        .asname
                        .as_ref()
                        .map(|n| n.as_str())
                        .unwrap_or_else(|| original_name.split('.').next().unwrap_or(""));
                    if let Some(module_path) = ModulePath::new(original_name) {
                        self.scope_visitor
                            .tracker
                            .bind_import(local_name, module_path, original_name);
                    } else {
                        self.scope_visitor
                            .tracker
                            .bind_name(local_name, super::BindingKind::Import);
                    }
                }
            }

            ast::Stmt::ImportFrom(import) => {
                let module_name = import.module.as_ref().map(|m| m.as_str());
                let level = import.level;
                let resolved_module = if level == 0 {
                    module_name.and_then(ModulePath::new)
                } else {
                    self.scope_visitor
                        .tracker
                        .resolve_import(module_name, level)
                };

                for alias in &import.names {
                    let original_name = alias.name.as_str();
                    let local_name = alias
                        .asname
                        .as_ref()
                        .map(|n| n.as_str())
                        .unwrap_or(original_name);
                    if local_name != "*" {
                        if let Some(ref module_path) = resolved_module {
                            self.scope_visitor
                                .tracker
                                .bind_import(local_name, module_path.clone(), original_name);
                        } else {
                            self.scope_visitor
                                .tracker
                                .bind_name(local_name, super::BindingKind::Import);
                        }
                    }
                }
            }

            // With statement binds optional vars
            ast::Stmt::With(with_stmt) => {
                for item in &with_stmt.items {
                    if let Some(vars) = &item.optional_vars {
                        self.bind_target(vars, super::BindingKind::WithVar);
                    }
                }
            }

            // Try/except binds exception names
            ast::Stmt::Try(try_stmt) => {
                for handler in &try_stmt.handlers {
                    let ast::ExceptHandler::ExceptHandler(h) = handler;
                    if let Some(name) = &h.name {
                        self.scope_visitor
                            .tracker
                            .bind_name(name.as_str(), super::BindingKind::ExceptVar);
                    }
                }
            }

            _ => {}
        }

        // Default: continue walking
        walk_stmt(self, stmt, path);
    }

    fn visit_expr(&mut self, expr: &'ast ast::Expr, path: &mut Path<'ast>) {
        // Check if this expression is a reference to a tracked import.
        // We check:
        // - Call expressions: Topic("name") - check the func
        // - Name expressions: @api (decorator without args)
        // - Subscript expressions: Topic[str]("name") - check the value
        let tracked_expr = match expr {
            ast::Expr::Call(call) => {
                // For calls, check if the function being called is tracked
                self.check_tracked_reference(&call.func)
                    .map(|module| (expr, module))
            }
            ast::Expr::Name(_) | ast::Expr::Subscript(_) => {
                // For bare names or subscripts, check if the expression itself is tracked.
                // BUT: skip if this is the func of a Call that we'll process separately.
                // This prevents double-counting @api(...) as both Call and Name.
                let is_call_func = path.parent().and_then(|p| p.as_expr()).is_some_and(|parent| {
                    matches!(parent, ast::Expr::Call(call) if std::ptr::eq(call.func.as_ref(), expr))
                });
                if is_call_func {
                    None
                } else {
                    self.check_tracked_reference(expr).map(|module| (expr, module))
                }
            }
            _ => None,
        };

        if let Some((tracked_expr, import_module)) = tracked_expr {
            let bind_name = self.extract_bind_name(path);
            let ctx = ReferenceContext {
                expr: tracked_expr,
                import_module,
                bind_name,
                path,
                file_id: self.file_id,
            };

            match R::parse_resource_reference(&ctx) {
                Ok(None) => {} // Not a resource reference, continue
                Ok(Some(r)) => self.results.push(Ok(r)),
                Err(e) => self.results.push(Err(e)),
            }
        }

        match expr {
            // Lambda creates a function scope
            ast::Expr::Lambda(lambda) => {
                self.scope_visitor.tracker.push_scope(ScopeKind::Function);

                if let Some(params) = &lambda.parameters {
                    self.bind_parameters(params);
                }

                walk_expr(self, expr, path);

                self.scope_visitor.tracker.pop_scope();
                return;
            }

            // Comprehensions create their own scope in Python 3
            ast::Expr::ListComp(comp) => {
                self.scope_visitor
                    .tracker
                    .push_scope(ScopeKind::Comprehension);
                for generator in &comp.generators {
                    self.bind_target(&generator.target, super::BindingKind::LoopVar);
                }
                walk_expr(self, expr, path);
                self.scope_visitor.tracker.pop_scope();
                return;
            }

            ast::Expr::SetComp(comp) => {
                self.scope_visitor
                    .tracker
                    .push_scope(ScopeKind::Comprehension);
                for generator in &comp.generators {
                    self.bind_target(&generator.target, super::BindingKind::LoopVar);
                }
                walk_expr(self, expr, path);
                self.scope_visitor.tracker.pop_scope();
                return;
            }

            ast::Expr::DictComp(comp) => {
                self.scope_visitor
                    .tracker
                    .push_scope(ScopeKind::Comprehension);
                for generator in &comp.generators {
                    self.bind_target(&generator.target, super::BindingKind::LoopVar);
                }
                walk_expr(self, expr, path);
                self.scope_visitor.tracker.pop_scope();
                return;
            }

            ast::Expr::Generator(gen) => {
                self.scope_visitor
                    .tracker
                    .push_scope(ScopeKind::Comprehension);
                for generator in &gen.generators {
                    self.bind_target(&generator.target, super::BindingKind::LoopVar);
                }
                walk_expr(self, expr, path);
                self.scope_visitor.tracker.pop_scope();
                return;
            }

            // Named expression (walrus operator := ) binds in enclosing non-comprehension scope
            ast::Expr::Named(named) => {
                let target_scope_idx = self
                    .scope_visitor
                    .tracker
                    .scopes()
                    .find(|(_, s)| s.kind != ScopeKind::Comprehension)
                    .map(|(idx, _)| idx)
                    .unwrap_or(0);

                if let ast::Expr::Name(name) = &*named.target {
                    let binding = super::Binding {
                        kind: super::BindingKind::Local,
                        scope_index: target_scope_idx,
                        import_from: None,
                        original_name: None,
                    };
                    self.scope_visitor.tracker.scopes[target_scope_idx]
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
mod tests {
    use super::*;
    use ruff_python_parser::parse_module;

    /// A simple test reference type for testing.
    #[derive(Debug, PartialEq)]
    struct TestRef {
        name: String,
        module: String,
        bind_name: Option<String>,
    }

    impl ReferenceParser<'_> for TestRef {
        fn parse_resource_reference(ctx: &ReferenceContext<'_, '_>) -> ParseResult<Option<Self>> {
            // Only match call expressions for this test
            if ctx.as_call().is_none() {
                return Ok(None);
            }

            let Some(name) = ctx.name() else {
                return Ok(None);
            };

            Ok(Some(TestRef {
                name: name.to_string(),
                module: ctx.import_module.as_str().to_string(),
                bind_name: ctx.bind_name.clone(),
            }))
        }
    }

    fn test_file_id() -> FileId {
        FileId::new(1)
    }

    fn find_refs(source: &str, tracked: &TrackedNames<'_>, module_path: Option<ModulePath>) -> Vec<ParseResult<TestRef>> {
        let parsed = parse_module(source).expect("parse");
        iter_references(
            parsed.syntax(),
            tracked,
            module_path,
            false,
            test_file_id(),
        )
    }

    #[test]
    fn test_simple_reference() {
        let source = r#"
from encoredev.pubsub import Topic

my_topic = Topic("events")
"#;
        let tracked = TrackedNames::new(&[("encoredev.pubsub", "Topic")]);
        let results = find_refs(source, &tracked, None);

        assert_eq!(results.len(), 1);
        let ref_ = results[0].as_ref().unwrap();
        assert_eq!(ref_.name, "Topic");
        assert_eq!(ref_.module, "encoredev.pubsub");
        assert_eq!(ref_.bind_name, Some("my_topic".to_string()));
    }

    #[test]
    fn test_shadowed_name_not_matched() {
        let source = r#"
from encoredev.pubsub import Topic

def foo():
    Topic = "not the real Topic"
    x = Topic("test")  # This should NOT match - Topic is shadowed by string
"#;
        let tracked = TrackedNames::new(&[("encoredev.pubsub", "Topic")]);
        let results = find_refs(source, &tracked, None);

        // Should have no matches because Topic is shadowed
        assert_eq!(results.len(), 0);
    }

    #[test]
    fn test_different_module_not_matched() {
        let source = r#"
from other.module import Topic

my_topic = Topic("events")
"#;
        let tracked = TrackedNames::new(&[("encoredev.pubsub", "Topic")]);
        let results = find_refs(source, &tracked, None);

        // Should have no matches because it's from a different module
        assert_eq!(results.len(), 0);
    }

    #[test]
    fn test_relative_import() {
        let source = r#"
from .pubsub import Topic

my_topic = Topic("events")
"#;
        let tracked = TrackedNames::new(&[("encoredev.pubsub", "Topic")]);
        let module_path = ModulePath::new("encoredev.service");
        let results = find_refs(source, &tracked, module_path);

        assert_eq!(results.len(), 1);
        let ref_ = results[0].as_ref().unwrap();
        assert_eq!(ref_.module, "encoredev.pubsub");
    }

    #[test]
    fn test_nested_scope_uses_outer_import() {
        let source = r#"
from encoredev.pubsub import Topic

def foo():
    # Topic here should resolve to the module-level import
    my_topic = Topic("events")
"#;
        let tracked = TrackedNames::new(&[("encoredev.pubsub", "Topic")]);
        let results = find_refs(source, &tracked, None);

        assert_eq!(results.len(), 1);
        let ref_ = results[0].as_ref().unwrap();
        assert_eq!(ref_.name, "Topic");
        assert_eq!(ref_.module, "encoredev.pubsub");
    }

    #[test]
    fn test_class_scope_reference() {
        let source = r#"
from encoredev.pubsub import Topic

class MyService:
    topic = Topic("events")
"#;
        let tracked = TrackedNames::new(&[("encoredev.pubsub", "Topic")]);
        let results = find_refs(source, &tracked, None);

        assert_eq!(results.len(), 1);
        let ref_ = results[0].as_ref().unwrap();
        assert_eq!(ref_.name, "Topic");
        assert_eq!(ref_.module, "encoredev.pubsub");
        assert_eq!(ref_.bind_name, Some("topic".to_string()));
    }

    #[test]
    fn test_comprehension_does_not_shadow_outer() {
        let source = r#"
from encoredev.pubsub import Topic

# Topic here is the loop var, NOT the import
items = [Topic for Topic in range(5)]

# Topic here should still be the import (comprehension scope ended)
my_topic = Topic("events")
"#;
        let tracked = TrackedNames::new(&[("encoredev.pubsub", "Topic")]);
        let results = find_refs(source, &tracked, None);

        // Should match the Topic("events") call, not the loop variable
        assert_eq!(results.len(), 1);
        let ref_ = results[0].as_ref().unwrap();
        assert_eq!(ref_.name, "Topic");
        assert_eq!(ref_.module, "encoredev.pubsub");
    }

    #[test]
    fn test_shadowed_in_comprehension_not_matched() {
        let source = r#"
from encoredev.pubsub import Topic

# Inside this comprehension, Topic is the loop variable, not the import
items = [Topic("x") for Topic in [str, int, float]]
"#;
        let tracked = TrackedNames::new(&[("encoredev.pubsub", "Topic")]);
        let results = find_refs(source, &tracked, None);

        // Should have NO matches because Topic inside comprehension is shadowed
        assert_eq!(results.len(), 0);
    }

    #[test]
    fn test_lambda_parameter_shadows() {
        let source = r#"
from encoredev.pubsub import Topic

# Topic is a parameter, shadows the import
f = lambda Topic: Topic("events")
"#;
        let tracked = TrackedNames::new(&[("encoredev.pubsub", "Topic")]);
        let results = find_refs(source, &tracked, None);

        // Should have NO matches because Topic is shadowed by lambda param
        assert_eq!(results.len(), 0);
    }

    #[test]
    fn test_aliased_import() {
        let source = r#"
from encoredev.pubsub import Topic as PubSubTopic

# Should be detected - aliased import from correct module
my_topic = PubSubTopic("events")
"#;
        let tracked = TrackedNames::new(&[("encoredev.pubsub", "Topic")]);
        let results = find_refs(source, &tracked, None);

        // Should match because original name is "Topic" from "encoredev.pubsub"
        assert_eq!(results.len(), 1);
        let ref_ = results[0].as_ref().unwrap();
        assert_eq!(ref_.name, "PubSubTopic"); // Local name used in code
        assert_eq!(ref_.module, "encoredev.pubsub");
        assert_eq!(ref_.bind_name, Some("my_topic".to_string()));
    }

    #[test]
    fn test_aliased_import_wrong_module() {
        let source = r#"
from other.module import Topic as OtherTopic

# Should NOT be detected - wrong module
my_topic = OtherTopic("events")
"#;
        let tracked = TrackedNames::new(&[("encoredev.pubsub", "Topic")]);
        let results = find_refs(source, &tracked, None);

        // Should NOT match because module is different
        assert_eq!(results.len(), 0);
    }

    // Tests for LiteralDict using pylitparser

    #[test]
    fn test_literal_dict_parse() {
        let source =
            r#"{"delivery_guarantee": "exactly-once", "ordering_attribute": "user_id", "count": 5}"#;
        let parsed = parse_module(source).expect("parse");
        let expr = &parsed.syntax().body[0];
        if let ast::Stmt::Expr(e) = expr {
            let dict = super::LiteralDict::parse(&e.value).unwrap();
            assert_eq!(
                dict.get::<String>("delivery_guarantee"),
                Some("exactly-once".to_string())
            );
            assert_eq!(
                dict.get::<String>("ordering_attribute"),
                Some("user_id".to_string())
            );
            assert_eq!(dict.get::<i64>("count"), Some(5));
            assert_eq!(dict.get::<String>("nonexistent"), None);
        } else {
            panic!("expected expression statement");
        }
    }

    #[test]
    fn test_literal_dict_with_non_literal_values() {
        // Dict with a variable reference should still parse the literal parts
        let source = r#"x = 1
{"key": x, "literal": "value"}"#;
        let parsed = parse_module(source).expect("parse");
        let expr = &parsed.syntax().body[1];
        if let ast::Stmt::Expr(e) = expr {
            let dict = super::LiteralDict::parse(&e.value).unwrap();
            // "key" has a non-literal value (variable x), so parsing fails
            assert_eq!(dict.get::<String>("key"), None);
            // "literal" has a literal value
            assert_eq!(dict.get::<String>("literal"), Some("value".to_string()));
        } else {
            panic!("expected expression statement");
        }
    }

    #[test]
    fn test_literal_dict_with_various_types() {
        let source = r#"{"name": "test", "count": 42, "enabled": True, "items": ["a", "b"]}"#;
        let parsed = parse_module(source).expect("parse");
        let expr = &parsed.syntax().body[0];
        if let ast::Stmt::Expr(e) = expr {
            let dict = super::LiteralDict::parse(&e.value).unwrap();
            assert_eq!(dict.get::<String>("name"), Some("test".to_string()));
            assert_eq!(dict.get::<i64>("count"), Some(42));
            assert_eq!(dict.get::<bool>("enabled"), Some(true));
            assert_eq!(
                dict.get::<Vec<String>>("items"),
                Some(vec!["a".to_string(), "b".to_string()])
            );
        } else {
            panic!("expected expression statement");
        }
    }

    // Tests for decorator detection

    /// A test type that only matches decorator usage
    #[derive(Debug, PartialEq)]
    struct DecoratorRef {
        name: String,
        func_name: String,
        has_args: bool,
    }

    impl ReferenceParser<'_> for DecoratorRef {
        fn parse_resource_reference(ctx: &ReferenceContext<'_, '_>) -> ParseResult<Option<Self>> {
            // Only match if used as a decorator
            let Some(func) = ctx.as_decorator() else {
                return Ok(None);
            };

            let Some(name) = ctx.name() else {
                return Ok(None);
            };

            Ok(Some(DecoratorRef {
                name: name.to_string(),
                func_name: func.name.to_string(),
                has_args: ctx.as_call().is_some(),
            }))
        }
    }

    fn find_decorator_refs(source: &str, tracked: &TrackedNames<'_>) -> Vec<ParseResult<DecoratorRef>> {
        let parsed = parse_module(source).expect("parse");
        iter_references(
            parsed.syntax(),
            tracked,
            None,
            false,
            test_file_id(),
        )
    }

    #[test]
    fn test_decorator_without_args() {
        let source = r#"
from encoredev.api import api

@api
def my_endpoint():
    pass
"#;
        let tracked = TrackedNames::new(&[("encoredev.api", "api")]);
        let results = find_decorator_refs(source, &tracked);

        assert_eq!(results.len(), 1);
        let ref_ = results[0].as_ref().unwrap();
        assert_eq!(ref_.name, "api");
        assert_eq!(ref_.func_name, "my_endpoint");
        assert!(!ref_.has_args);
    }

    #[test]
    fn test_decorator_with_args() {
        let source = r#"
from encoredev.api import api

@api(method="GET", path="/users")
def get_users():
    pass
"#;
        let tracked = TrackedNames::new(&[("encoredev.api", "api")]);
        let results = find_decorator_refs(source, &tracked);

        assert_eq!(results.len(), 1);
        let ref_ = results[0].as_ref().unwrap();
        assert_eq!(ref_.name, "api");
        assert_eq!(ref_.func_name, "get_users");
        assert!(ref_.has_args);
    }

    #[test]
    fn test_non_decorator_call_ignored() {
        let source = r#"
from encoredev.api import api

# This is NOT a decorator usage - should be ignored
result = api(method="GET")

@api
def my_endpoint():
    pass
"#;
        let tracked = TrackedNames::new(&[("encoredev.api", "api")]);
        let results = find_decorator_refs(source, &tracked);

        // Should only match the decorator, not the regular call
        assert_eq!(results.len(), 1);
        let ref_ = results[0].as_ref().unwrap();
        assert_eq!(ref_.func_name, "my_endpoint");
    }

    #[test]
    fn test_shadowed_decorator_not_matched() {
        let source = r#"
from encoredev.api import api

def outer():
    api = lambda f: f  # Shadow the import

    @api  # This uses the local lambda, not the import
    def inner():
        pass
"#;
        let tracked = TrackedNames::new(&[("encoredev.api", "api")]);
        let results = find_decorator_refs(source, &tracked);

        // Should have no matches because api is shadowed
        assert_eq!(results.len(), 0);
    }

    #[test]
    fn test_multiple_decorators() {
        let source = r#"
from encoredev.api import api

@api
def endpoint1():
    pass

@api(method="POST")
def endpoint2():
    pass
"#;
        let tracked = TrackedNames::new(&[("encoredev.api", "api")]);
        let results = find_decorator_refs(source, &tracked);

        assert_eq!(results.len(), 2);
        let names: Vec<_> = results.iter().map(|r| r.as_ref().unwrap().func_name.as_str()).collect();
        assert!(names.contains(&"endpoint1"));
        assert!(names.contains(&"endpoint2"));
    }

    #[test]
    fn test_aliased_decorator() {
        let source = r#"
from encoredev.api import api as endpoint

@endpoint(method="GET")
def get_data():
    pass
"#;
        let tracked = TrackedNames::new(&[("encoredev.api", "api")]);
        let results = find_decorator_refs(source, &tracked);

        assert_eq!(results.len(), 1);
        let ref_ = results[0].as_ref().unwrap();
        assert_eq!(ref_.name, "endpoint");  // Uses local alias
        assert_eq!(ref_.func_name, "get_data");
    }
}
