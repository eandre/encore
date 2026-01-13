//! AST visitor with path tracking.
//!
//! This module provides a visitor pattern for traversing Python ASTs
//! while tracking the path (ancestry) to the current node.

use ruff_python_ast as ast;

/// A node in the AST path, representing an ancestor of the current node.
#[derive(Debug, Clone, Copy)]
pub enum PathNode<'a> {
    Module(&'a ast::ModModule),
    Stmt(&'a ast::Stmt),
    Expr(&'a ast::Expr),
    Comprehension(&'a ast::Comprehension),
    ExceptHandler(&'a ast::ExceptHandler),
    Parameter(&'a ast::Parameter),
    Keyword(&'a ast::Keyword),
    Alias(&'a ast::Alias),
    WithItem(&'a ast::WithItem),
    MatchCase(&'a ast::MatchCase),
    Decorator(&'a ast::Decorator),
    TypeParam(&'a ast::TypeParam),
}

impl<'a> PathNode<'a> {
    /// Returns this node as a statement, if it is one.
    pub fn as_stmt(&self) -> Option<&'a ast::Stmt> {
        match self {
            PathNode::Stmt(s) => Some(s),
            _ => None,
        }
    }

    /// Returns this node as an expression, if it is one.
    pub fn as_expr(&self) -> Option<&'a ast::Expr> {
        match self {
            PathNode::Expr(e) => Some(e),
            _ => None,
        }
    }

    /// Returns this node as a function definition, if it is one.
    pub fn as_function_def(&self) -> Option<&'a ast::StmtFunctionDef> {
        match self {
            PathNode::Stmt(ast::Stmt::FunctionDef(f)) => Some(f),
            _ => None,
        }
    }

    /// Returns this node as a class definition, if it is one.
    pub fn as_class_def(&self) -> Option<&'a ast::StmtClassDef> {
        match self {
            PathNode::Stmt(ast::Stmt::ClassDef(c)) => Some(c),
            _ => None,
        }
    }
}

/// The path from the root to the current node being visited.
#[derive(Debug, Clone, Default)]
pub struct Path<'a> {
    nodes: Vec<PathNode<'a>>,
}

impl<'a> Path<'a> {
    /// Creates a new empty path.
    pub fn new() -> Self {
        Path { nodes: Vec::new() }
    }

    /// Returns the number of nodes in the path.
    pub fn len(&self) -> usize {
        self.nodes.len()
    }

    /// Returns true if the path is empty.
    pub fn is_empty(&self) -> bool {
        self.nodes.is_empty()
    }

    /// Returns the parent node (immediate ancestor), if any.
    pub fn parent(&self) -> Option<&PathNode<'a>> {
        self.nodes.last()
    }

    /// Returns the grandparent node, if any.
    pub fn grandparent(&self) -> Option<&PathNode<'a>> {
        if self.nodes.len() >= 2 {
            self.nodes.get(self.nodes.len() - 2)
        } else {
            None
        }
    }

    /// Returns an iterator over the path from root to parent.
    pub fn iter(&self) -> impl Iterator<Item = &PathNode<'a>> {
        self.nodes.iter()
    }

    /// Returns an iterator over the path from parent to root.
    pub fn iter_ancestors(&self) -> impl Iterator<Item = &PathNode<'a>> {
        self.nodes.iter().rev()
    }

    /// Returns the ancestor at the given index (0 = parent, 1 = grandparent, etc.)
    pub fn ancestor(&self, index: usize) -> Option<&PathNode<'a>> {
        if index < self.nodes.len() {
            self.nodes.get(self.nodes.len() - 1 - index)
        } else {
            None
        }
    }

    /// Finds the first ancestor matching the predicate, searching from parent to root.
    pub fn find_ancestor<F>(&self, predicate: F) -> Option<&PathNode<'a>>
    where
        F: Fn(&PathNode<'a>) -> bool,
    {
        self.iter_ancestors().find(|n| predicate(n))
    }

    /// Returns the nearest enclosing function definition, if any.
    pub fn enclosing_function(&self) -> Option<&'a ast::StmtFunctionDef> {
        self.iter_ancestors().find_map(|n| n.as_function_def())
    }

    /// Returns the nearest enclosing class definition, if any.
    pub fn enclosing_class(&self) -> Option<&'a ast::StmtClassDef> {
        self.iter_ancestors().find_map(|n| n.as_class_def())
    }

    /// Returns true if currently inside a function.
    pub fn in_function(&self) -> bool {
        self.enclosing_function().is_some()
    }

    /// Returns true if currently inside a class.
    pub fn in_class(&self) -> bool {
        self.enclosing_class().is_some()
    }

    /// Pushes a node onto the path.
    fn push(&mut self, node: PathNode<'a>) {
        self.nodes.push(node);
    }

    /// Pops the last node from the path.
    fn pop(&mut self) -> Option<PathNode<'a>> {
        self.nodes.pop()
    }
}

/// A visitor that traverses the AST while tracking the path to each node.
///
/// Implement this trait to visit nodes while having access to their ancestry.
/// Each `visit_*` method is called when entering a node, and each `exit_*`
/// method is called when leaving a node.
///
/// By default, all methods call `walk_*` to continue traversal. Override
/// specific methods to add custom behavior.
pub trait PathVisitor<'a> {
    /// Called when entering a module.
    fn visit_module(&mut self, module: &'a ast::ModModule, path: &mut Path<'a>) {
        walk_module(self, module, path);
    }

    /// Called when exiting a module.
    fn exit_module(&mut self, _module: &'a ast::ModModule, _path: &mut Path<'a>) {}

    /// Called when entering a statement.
    fn visit_stmt(&mut self, stmt: &'a ast::Stmt, path: &mut Path<'a>) {
        walk_stmt(self, stmt, path);
    }

    /// Called when exiting a statement.
    fn exit_stmt(&mut self, _stmt: &'a ast::Stmt, _path: &mut Path<'a>) {}

    /// Called when entering an expression.
    fn visit_expr(&mut self, expr: &'a ast::Expr, path: &mut Path<'a>) {
        walk_expr(self, expr, path);
    }

    /// Called when exiting an expression.
    fn exit_expr(&mut self, _expr: &'a ast::Expr, _path: &mut Path<'a>) {}

    /// Called when entering a comprehension.
    fn visit_comprehension(&mut self, comp: &'a ast::Comprehension, path: &mut Path<'a>) {
        walk_comprehension(self, comp, path);
    }

    /// Called when exiting a comprehension.
    fn exit_comprehension(&mut self, _comp: &'a ast::Comprehension, _path: &mut Path<'a>) {}

    /// Called when entering an except handler.
    fn visit_except_handler(&mut self, handler: &'a ast::ExceptHandler, path: &mut Path<'a>) {
        walk_except_handler(self, handler, path);
    }

    /// Called when exiting an except handler.
    fn exit_except_handler(&mut self, _handler: &'a ast::ExceptHandler, _path: &mut Path<'a>) {}

    /// Called when entering a parameter.
    fn visit_parameter(&mut self, param: &'a ast::Parameter, path: &mut Path<'a>) {
        walk_parameter(self, param, path);
    }

    /// Called when exiting a parameter.
    fn exit_parameter(&mut self, _param: &'a ast::Parameter, _path: &mut Path<'a>) {}

    /// Called when entering a keyword argument.
    fn visit_keyword(&mut self, keyword: &'a ast::Keyword, path: &mut Path<'a>) {
        walk_keyword(self, keyword, path);
    }

    /// Called when exiting a keyword argument.
    fn exit_keyword(&mut self, _keyword: &'a ast::Keyword, _path: &mut Path<'a>) {}

    /// Called when entering an alias (import).
    fn visit_alias(&mut self, _alias: &'a ast::Alias, _path: &mut Path<'a>) {}

    /// Called when exiting an alias (import).
    fn exit_alias(&mut self, _alias: &'a ast::Alias, _path: &mut Path<'a>) {}

    /// Called when entering a with item.
    fn visit_with_item(&mut self, item: &'a ast::WithItem, path: &mut Path<'a>) {
        walk_with_item(self, item, path);
    }

    /// Called when exiting a with item.
    fn exit_with_item(&mut self, _item: &'a ast::WithItem, _path: &mut Path<'a>) {}

    /// Called when entering a match case.
    fn visit_match_case(&mut self, case: &'a ast::MatchCase, path: &mut Path<'a>) {
        walk_match_case(self, case, path);
    }

    /// Called when exiting a match case.
    fn exit_match_case(&mut self, _case: &'a ast::MatchCase, _path: &mut Path<'a>) {}

    /// Called when entering a decorator.
    fn visit_decorator(&mut self, decorator: &'a ast::Decorator, path: &mut Path<'a>) {
        walk_decorator(self, decorator, path);
    }

    /// Called when exiting a decorator.
    fn exit_decorator(&mut self, _decorator: &'a ast::Decorator, _path: &mut Path<'a>) {}

    /// Called when entering a type parameter.
    fn visit_type_param(&mut self, param: &'a ast::TypeParam, path: &mut Path<'a>) {
        walk_type_param(self, param, path);
    }

    /// Called when exiting a type parameter.
    fn exit_type_param(&mut self, _param: &'a ast::TypeParam, _path: &mut Path<'a>) {}

    /// Called when entering a pattern (for match statements).
    fn visit_pattern(&mut self, pattern: &'a ast::Pattern, path: &mut Path<'a>) {
        walk_pattern(self, pattern, path);
    }

    /// Called when exiting a pattern (for match statements).
    fn exit_pattern(&mut self, _pattern: &'a ast::Pattern, _path: &mut Path<'a>) {}
}

/// Walk a module and its children.
pub fn walk_module<'a, V: PathVisitor<'a> + ?Sized>(
    visitor: &mut V,
    module: &'a ast::ModModule,
    path: &mut Path<'a>,
) {
    path.push(PathNode::Module(module));
    for stmt in &module.body {
        visitor.visit_stmt(stmt, path);
    }
    visitor.exit_module(module, path);
    path.pop();
}

/// Walk a statement and its children.
pub fn walk_stmt<'a, V: PathVisitor<'a> + ?Sized>(
    visitor: &mut V,
    stmt: &'a ast::Stmt,
    path: &mut Path<'a>,
) {
    path.push(PathNode::Stmt(stmt));

    match stmt {
        ast::Stmt::FunctionDef(node) => {
            for decorator in &node.decorator_list {
                visitor.visit_decorator(decorator, path);
            }
            if let Some(type_params) = &node.type_params {
                for param in &type_params.type_params {
                    visitor.visit_type_param(param, path);
                }
            }
            for param in node
                .parameters
                .posonlyargs
                .iter()
                .chain(node.parameters.args.iter())
                .chain(node.parameters.kwonlyargs.iter())
            {
                visitor.visit_parameter(&param.parameter, path);
            }
            if let Some(param) = &node.parameters.vararg {
                visitor.visit_parameter(param, path);
            }
            if let Some(param) = &node.parameters.kwarg {
                visitor.visit_parameter(param, path);
            }
            if let Some(returns) = &node.returns {
                visitor.visit_expr(returns, path);
            }
            for stmt in &node.body {
                visitor.visit_stmt(stmt, path);
            }
        }
        ast::Stmt::ClassDef(node) => {
            for decorator in &node.decorator_list {
                visitor.visit_decorator(decorator, path);
            }
            if let Some(type_params) = &node.type_params {
                for param in &type_params.type_params {
                    visitor.visit_type_param(param, path);
                }
            }
            if let Some(arguments) = &node.arguments {
                for arg in &arguments.args {
                    visitor.visit_expr(arg, path);
                }
                for keyword in &arguments.keywords {
                    visitor.visit_keyword(keyword, path);
                }
            }
            for stmt in &node.body {
                visitor.visit_stmt(stmt, path);
            }
        }
        ast::Stmt::Return(node) => {
            if let Some(value) = &node.value {
                visitor.visit_expr(value, path);
            }
        }
        ast::Stmt::Delete(node) => {
            for target in &node.targets {
                visitor.visit_expr(target, path);
            }
        }
        ast::Stmt::Assign(node) => {
            for target in &node.targets {
                visitor.visit_expr(target, path);
            }
            visitor.visit_expr(&node.value, path);
        }
        ast::Stmt::AugAssign(node) => {
            visitor.visit_expr(&node.target, path);
            visitor.visit_expr(&node.value, path);
        }
        ast::Stmt::AnnAssign(node) => {
            visitor.visit_expr(&node.target, path);
            visitor.visit_expr(&node.annotation, path);
            if let Some(value) = &node.value {
                visitor.visit_expr(value, path);
            }
        }
        ast::Stmt::TypeAlias(node) => {
            visitor.visit_expr(&node.name, path);
            if let Some(type_params) = &node.type_params {
                for param in &type_params.type_params {
                    visitor.visit_type_param(param, path);
                }
            }
            visitor.visit_expr(&node.value, path);
        }
        ast::Stmt::For(node) => {
            visitor.visit_expr(&node.target, path);
            visitor.visit_expr(&node.iter, path);
            for stmt in &node.body {
                visitor.visit_stmt(stmt, path);
            }
            for stmt in &node.orelse {
                visitor.visit_stmt(stmt, path);
            }
        }
        ast::Stmt::While(node) => {
            visitor.visit_expr(&node.test, path);
            for stmt in &node.body {
                visitor.visit_stmt(stmt, path);
            }
            for stmt in &node.orelse {
                visitor.visit_stmt(stmt, path);
            }
        }
        ast::Stmt::If(node) => {
            visitor.visit_expr(&node.test, path);
            for stmt in &node.body {
                visitor.visit_stmt(stmt, path);
            }
            for clause in &node.elif_else_clauses {
                if let Some(test) = &clause.test {
                    visitor.visit_expr(test, path);
                }
                for stmt in &clause.body {
                    visitor.visit_stmt(stmt, path);
                }
            }
        }
        ast::Stmt::With(node) => {
            for item in &node.items {
                visitor.visit_with_item(item, path);
            }
            for stmt in &node.body {
                visitor.visit_stmt(stmt, path);
            }
        }
        ast::Stmt::Match(node) => {
            visitor.visit_expr(&node.subject, path);
            for case in &node.cases {
                visitor.visit_match_case(case, path);
            }
        }
        ast::Stmt::Raise(node) => {
            if let Some(exc) = &node.exc {
                visitor.visit_expr(exc, path);
            }
            if let Some(cause) = &node.cause {
                visitor.visit_expr(cause, path);
            }
        }
        ast::Stmt::Try(node) => {
            for stmt in &node.body {
                visitor.visit_stmt(stmt, path);
            }
            for handler in &node.handlers {
                visitor.visit_except_handler(handler, path);
            }
            for stmt in &node.orelse {
                visitor.visit_stmt(stmt, path);
            }
            for stmt in &node.finalbody {
                visitor.visit_stmt(stmt, path);
            }
        }
        ast::Stmt::Assert(node) => {
            visitor.visit_expr(&node.test, path);
            if let Some(msg) = &node.msg {
                visitor.visit_expr(msg, path);
            }
        }
        ast::Stmt::Import(node) => {
            for alias in &node.names {
                visitor.visit_alias(alias, path);
            }
        }
        ast::Stmt::ImportFrom(node) => {
            for alias in &node.names {
                visitor.visit_alias(alias, path);
            }
        }
        ast::Stmt::Global(_) | ast::Stmt::Nonlocal(_) => {}
        ast::Stmt::Expr(node) => {
            visitor.visit_expr(&node.value, path);
        }
        ast::Stmt::Pass(_) | ast::Stmt::Break(_) | ast::Stmt::Continue(_) => {}
        ast::Stmt::IpyEscapeCommand(_) => {}
    }

    visitor.exit_stmt(stmt, path);
    path.pop();
}

/// Walk an expression and its children.
pub fn walk_expr<'a, V: PathVisitor<'a> + ?Sized>(
    visitor: &mut V,
    expr: &'a ast::Expr,
    path: &mut Path<'a>,
) {
    path.push(PathNode::Expr(expr));

    match expr {
        ast::Expr::BoolOp(node) => {
            for value in &node.values {
                visitor.visit_expr(value, path);
            }
        }
        ast::Expr::Named(node) => {
            visitor.visit_expr(&node.target, path);
            visitor.visit_expr(&node.value, path);
        }
        ast::Expr::BinOp(node) => {
            visitor.visit_expr(&node.left, path);
            visitor.visit_expr(&node.right, path);
        }
        ast::Expr::UnaryOp(node) => {
            visitor.visit_expr(&node.operand, path);
        }
        ast::Expr::Lambda(node) => {
            if let Some(parameters) = &node.parameters {
                for param in parameters
                    .posonlyargs
                    .iter()
                    .chain(parameters.args.iter())
                    .chain(parameters.kwonlyargs.iter())
                {
                    visitor.visit_parameter(&param.parameter, path);
                }
                if let Some(param) = &parameters.vararg {
                    visitor.visit_parameter(param, path);
                }
                if let Some(param) = &parameters.kwarg {
                    visitor.visit_parameter(param, path);
                }
            }
            visitor.visit_expr(&node.body, path);
        }
        ast::Expr::If(node) => {
            visitor.visit_expr(&node.test, path);
            visitor.visit_expr(&node.body, path);
            visitor.visit_expr(&node.orelse, path);
        }
        ast::Expr::Dict(node) => {
            for item in node.items.iter() {
                if let Some(key) = &item.key {
                    visitor.visit_expr(key, path);
                }
                visitor.visit_expr(&item.value, path);
            }
        }
        ast::Expr::Set(node) => {
            for elt in &node.elts {
                visitor.visit_expr(elt, path);
            }
        }
        ast::Expr::ListComp(node) => {
            visitor.visit_expr(&node.elt, path);
            for comp in &node.generators {
                visitor.visit_comprehension(comp, path);
            }
        }
        ast::Expr::SetComp(node) => {
            visitor.visit_expr(&node.elt, path);
            for comp in &node.generators {
                visitor.visit_comprehension(comp, path);
            }
        }
        ast::Expr::DictComp(node) => {
            visitor.visit_expr(&node.key, path);
            visitor.visit_expr(&node.value, path);
            for comp in &node.generators {
                visitor.visit_comprehension(comp, path);
            }
        }
        ast::Expr::Generator(node) => {
            visitor.visit_expr(&node.elt, path);
            for comp in &node.generators {
                visitor.visit_comprehension(comp, path);
            }
        }
        ast::Expr::Await(node) => {
            visitor.visit_expr(&node.value, path);
        }
        ast::Expr::Yield(node) => {
            if let Some(value) = &node.value {
                visitor.visit_expr(value, path);
            }
        }
        ast::Expr::YieldFrom(node) => {
            visitor.visit_expr(&node.value, path);
        }
        ast::Expr::Compare(node) => {
            visitor.visit_expr(&node.left, path);
            for comparator in &node.comparators {
                visitor.visit_expr(comparator, path);
            }
        }
        ast::Expr::Call(node) => {
            visitor.visit_expr(&node.func, path);
            for arg in &node.arguments.args {
                visitor.visit_expr(arg, path);
            }
            for keyword in &node.arguments.keywords {
                visitor.visit_keyword(keyword, path);
            }
        }
        ast::Expr::FString(node) => {
            for part in node.value.elements() {
                match part {
                    ast::FStringElement::Literal(_) => {}
                    ast::FStringElement::Expression(expr) => {
                        visitor.visit_expr(&expr.expression, path);
                        if let Some(format_spec) = &expr.format_spec {
                            for element in &format_spec.elements {
                                if let ast::FStringElement::Expression(e) = element {
                                    visitor.visit_expr(&e.expression, path);
                                }
                            }
                        }
                    }
                }
            }
        }
        ast::Expr::StringLiteral(_)
        | ast::Expr::BytesLiteral(_)
        | ast::Expr::NumberLiteral(_)
        | ast::Expr::BooleanLiteral(_)
        | ast::Expr::NoneLiteral(_)
        | ast::Expr::EllipsisLiteral(_) => {}
        ast::Expr::Attribute(node) => {
            visitor.visit_expr(&node.value, path);
        }
        ast::Expr::Subscript(node) => {
            visitor.visit_expr(&node.value, path);
            visitor.visit_expr(&node.slice, path);
        }
        ast::Expr::Starred(node) => {
            visitor.visit_expr(&node.value, path);
        }
        ast::Expr::Name(_) => {}
        ast::Expr::List(node) => {
            for elt in &node.elts {
                visitor.visit_expr(elt, path);
            }
        }
        ast::Expr::Tuple(node) => {
            for elt in &node.elts {
                visitor.visit_expr(elt, path);
            }
        }
        ast::Expr::Slice(node) => {
            if let Some(lower) = &node.lower {
                visitor.visit_expr(lower, path);
            }
            if let Some(upper) = &node.upper {
                visitor.visit_expr(upper, path);
            }
            if let Some(step) = &node.step {
                visitor.visit_expr(step, path);
            }
        }
        ast::Expr::IpyEscapeCommand(_) => {}
    }

    visitor.exit_expr(expr, path);
    path.pop();
}

/// Walk a comprehension and its children.
pub fn walk_comprehension<'a, V: PathVisitor<'a> + ?Sized>(
    visitor: &mut V,
    comp: &'a ast::Comprehension,
    path: &mut Path<'a>,
) {
    path.push(PathNode::Comprehension(comp));

    visitor.visit_expr(&comp.target, path);
    visitor.visit_expr(&comp.iter, path);
    for condition in &comp.ifs {
        visitor.visit_expr(condition, path);
    }

    visitor.exit_comprehension(comp, path);
    path.pop();
}

/// Walk an except handler and its children.
pub fn walk_except_handler<'a, V: PathVisitor<'a> + ?Sized>(
    visitor: &mut V,
    handler: &'a ast::ExceptHandler,
    path: &mut Path<'a>,
) {
    path.push(PathNode::ExceptHandler(handler));

    let ast::ExceptHandler::ExceptHandler(node) = handler;
    if let Some(ty) = &node.type_ {
        visitor.visit_expr(ty, path);
    }
    for stmt in &node.body {
        visitor.visit_stmt(stmt, path);
    }

    visitor.exit_except_handler(handler, path);
    path.pop();
}

/// Walk a parameter and its children.
pub fn walk_parameter<'a, V: PathVisitor<'a> + ?Sized>(
    visitor: &mut V,
    param: &'a ast::Parameter,
    path: &mut Path<'a>,
) {
    path.push(PathNode::Parameter(param));

    if let Some(annotation) = &param.annotation {
        visitor.visit_expr(annotation, path);
    }

    visitor.exit_parameter(param, path);
    path.pop();
}

/// Walk a keyword argument and its children.
pub fn walk_keyword<'a, V: PathVisitor<'a> + ?Sized>(
    visitor: &mut V,
    keyword: &'a ast::Keyword,
    path: &mut Path<'a>,
) {
    path.push(PathNode::Keyword(keyword));

    visitor.visit_expr(&keyword.value, path);

    visitor.exit_keyword(keyword, path);
    path.pop();
}

/// Walk a with item and its children.
pub fn walk_with_item<'a, V: PathVisitor<'a> + ?Sized>(
    visitor: &mut V,
    item: &'a ast::WithItem,
    path: &mut Path<'a>,
) {
    path.push(PathNode::WithItem(item));

    visitor.visit_expr(&item.context_expr, path);
    if let Some(vars) = &item.optional_vars {
        visitor.visit_expr(vars, path);
    }

    visitor.exit_with_item(item, path);
    path.pop();
}

/// Walk a match case and its children.
pub fn walk_match_case<'a, V: PathVisitor<'a> + ?Sized>(
    visitor: &mut V,
    case: &'a ast::MatchCase,
    path: &mut Path<'a>,
) {
    path.push(PathNode::MatchCase(case));

    visitor.visit_pattern(&case.pattern, path);
    if let Some(guard) = &case.guard {
        visitor.visit_expr(guard, path);
    }
    for stmt in &case.body {
        visitor.visit_stmt(stmt, path);
    }

    visitor.exit_match_case(case, path);
    path.pop();
}

/// Walk a decorator and its children.
pub fn walk_decorator<'a, V: PathVisitor<'a> + ?Sized>(
    visitor: &mut V,
    decorator: &'a ast::Decorator,
    path: &mut Path<'a>,
) {
    path.push(PathNode::Decorator(decorator));

    visitor.visit_expr(&decorator.expression, path);

    visitor.exit_decorator(decorator, path);
    path.pop();
}

/// Walk a type parameter and its children.
pub fn walk_type_param<'a, V: PathVisitor<'a> + ?Sized>(
    visitor: &mut V,
    param: &'a ast::TypeParam,
    path: &mut Path<'a>,
) {
    path.push(PathNode::TypeParam(param));

    match param {
        ast::TypeParam::TypeVar(node) => {
            if let Some(bound) = &node.bound {
                visitor.visit_expr(bound, path);
            }
            if let Some(default) = &node.default {
                visitor.visit_expr(default, path);
            }
        }
        ast::TypeParam::TypeVarTuple(node) => {
            if let Some(default) = &node.default {
                visitor.visit_expr(default, path);
            }
        }
        ast::TypeParam::ParamSpec(node) => {
            if let Some(default) = &node.default {
                visitor.visit_expr(default, path);
            }
        }
    }

    visitor.exit_type_param(param, path);
    path.pop();
}

/// Walk a pattern and its children.
pub fn walk_pattern<'a, V: PathVisitor<'a> + ?Sized>(
    visitor: &mut V,
    pattern: &'a ast::Pattern,
    path: &mut Path<'a>,
) {
    match pattern {
        ast::Pattern::MatchValue(node) => {
            visitor.visit_expr(&node.value, path);
        }
        ast::Pattern::MatchSingleton(_) => {}
        ast::Pattern::MatchSequence(node) => {
            for pat in &node.patterns {
                visitor.visit_pattern(pat, path);
            }
        }
        ast::Pattern::MatchMapping(node) => {
            for key in &node.keys {
                visitor.visit_expr(key, path);
            }
            for pat in &node.patterns {
                visitor.visit_pattern(pat, path);
            }
        }
        ast::Pattern::MatchClass(node) => {
            visitor.visit_expr(&node.cls, path);
            for pat in &node.arguments.patterns {
                visitor.visit_pattern(pat, path);
            }
            for kw in &node.arguments.keywords {
                visitor.visit_pattern(&kw.pattern, path);
            }
        }
        ast::Pattern::MatchStar(_) => {}
        ast::Pattern::MatchAs(node) => {
            if let Some(pat) = &node.pattern {
                visitor.visit_pattern(pat, path);
            }
        }
        ast::Pattern::MatchOr(node) => {
            for pat in &node.patterns {
                visitor.visit_pattern(pat, path);
            }
        }
    }

    visitor.exit_pattern(pattern, path);
}
