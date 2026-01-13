//! Parsing utilities for resource discovery.

use ruff_python_ast as ast;

use crate::ast::visitor::Path;

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

/// Extracts a list of strings from an expression.
pub fn extract_string_list(expr: &ast::Expr) -> Option<Vec<String>> {
    match expr {
        ast::Expr::List(list) => {
            let mut result = Vec::new();
            for elem in &list.elts {
                if let Some(s) = extract_string(elem) {
                    result.push(s);
                } else {
                    return None;
                }
            }
            Some(result)
        }
        _ => None,
    }
}

/// Extracts keyword argument values from a call expression.
pub struct KeywordArgs<'a> {
    keywords: &'a [ast::Keyword],
}

impl<'a> KeywordArgs<'a> {
    /// Creates a new KeywordArgs helper.
    pub fn new(keywords: &'a [ast::Keyword]) -> Self {
        KeywordArgs { keywords }
    }

    /// Gets a keyword argument by name.
    pub fn get(&self, name: &str) -> Option<&'a ast::Expr> {
        for kw in self.keywords {
            if let Some(arg) = &kw.arg {
                if arg.as_str() == name {
                    return Some(&kw.value);
                }
            }
        }
        None
    }

    /// Gets a string keyword argument.
    pub fn get_string(&self, name: &str) -> Option<String> {
        self.get(name).and_then(extract_string)
    }

    /// Gets a boolean keyword argument.
    pub fn get_bool(&self, name: &str) -> Option<bool> {
        self.get(name).and_then(extract_bool)
    }

    /// Gets an integer keyword argument.
    pub fn get_int(&self, name: &str) -> Option<i64> {
        self.get(name).and_then(extract_int)
    }

    /// Gets a string list keyword argument.
    pub fn get_string_list(&self, name: &str) -> Option<Vec<String>> {
        self.get(name).and_then(extract_string_list)
    }
}

/// Checks if a call is to a specific class constructor.
pub fn is_class_call(call: &ast::ExprCall, class_name: &str) -> bool {
    let func = &*call.func;
    log::info!("got call func {func:?}");
    match &*call.func {
        ast::Expr::Name(name) => name.id.as_str() == class_name,
        ast::Expr::Subscript(sub) => {
            // Handle generic types like Topic[T]
            if let ast::Expr::Name(name) = &*sub.value {
                name.id.as_str() == class_name
            } else {
                false
            }
        }
        _ => false,
    }
}

/// Checks if a decorator matches a function call pattern.
pub fn is_decorator_call(decorator: &ast::Decorator, name: &str) -> bool {
    match &decorator.expression {
        ast::Expr::Name(n) => n.id.as_str() == name,
        ast::Expr::Attribute(attr) => get_full_attribute_name(attr) == name,
        ast::Expr::Call(call) => match &*call.func {
            ast::Expr::Name(n) => n.id.as_str() == name,
            ast::Expr::Attribute(attr) => get_full_attribute_name(attr) == name,
            _ => false,
        },
        _ => false,
    }
}

/// Extracts the call expression from a decorator.
pub fn get_decorator_call(decorator: &ast::Decorator) -> Option<&ast::ExprCall> {
    match &decorator.expression {
        ast::Expr::Call(call) => Some(call),
        _ => None,
    }
}

/// Gets the full dotted name from an attribute expression.
pub fn get_full_attribute_name(attr: &ast::ExprAttribute) -> String {
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

/// Checks if a name is imported from a specific module.
pub fn is_imported_from(imports: &[(String, String)], name: &str, module: &str) -> bool {
    for (import_name, import_module) in imports {
        if import_name == name && import_module == module {
            return true;
        }
    }
    false
}

/// Extracts positional arguments from a call.
pub fn get_positional_args(call: &ast::ExprCall) -> &[ast::Expr] {
    &call.arguments.args
}

/// Gets the first positional argument as a string.
pub fn get_first_string_arg(call: &ast::ExprCall) -> Option<String> {
    call.arguments.args.first().and_then(extract_string)
}

/// Gets the first positional argument.
pub fn get_first_arg(call: &ast::ExprCall) -> Option<&ast::Expr> {
    call.arguments.args.first()
}

/// Gets the second positional argument.
pub fn get_second_arg(call: &ast::ExprCall) -> Option<&ast::Expr> {
    call.arguments.args.get(1)
}

/// Extracts the bind name from the path (variable being assigned to).
///
/// Looks for assignment patterns in the path to determine what name
/// the resource is being assigned to.
pub fn extract_bind_name(path: &Path<'_>) -> Option<String> {
    for node in path.iter_ancestors() {
        if let Some(stmt) = node.as_stmt() {
            match stmt {
                ast::Stmt::Assign(assign) => {
                    // Handle `x = ...`
                    if assign.targets.len() == 1 {
                        if let ast::Expr::Name(name) = &assign.targets[0] {
                            return Some(name.id.to_string());
                        }
                    }
                }
                ast::Stmt::AnnAssign(ann_assign) => {
                    // Handle `x: Type = ...`
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

/// Validates that a resource name follows snake_case naming conventions.
///
/// Snake case names must:
/// - Be between 1 and 63 characters long
/// - Start with a lowercase letter
/// - End with a lowercase letter or number
/// - Only contain lowercase letters, numbers, and underscores
/// - Not start with the reserved prefix (if provided)
///
/// Returns `Ok(())` if valid, or an error string if invalid.
pub fn validate_snake_case_name(name: &str, reserved_prefix: Option<&str>) -> Result<(), String> {
    const MAX_LENGTH: usize = 63;

    // Check length
    if name.is_empty() || name.len() > MAX_LENGTH {
        return Err(format!(
            "name must be between 1 and {} characters long (got {})",
            MAX_LENGTH,
            name.len()
        ));
    }

    // Check snake_case format: ^[a-z]([_a-z0-9]*[a-z0-9])?$
    let mut chars = name.chars();

    // First character must be a lowercase letter
    let first = chars.next().unwrap();
    if !first.is_ascii_lowercase() {
        return Err(format!(
            "name must start with a lowercase letter (got '{}')",
            first
        ));
    }

    // If there's only one character, it's valid
    if name.len() == 1 {
        // Check reserved prefix
        if let Some(prefix) = reserved_prefix {
            if name.starts_with(prefix) {
                return Err(format!(
                    "name must not start with reserved prefix '{}' (got '{}')",
                    prefix, name
                ));
            }
        }
        return Ok(());
    }

    // Last character must be lowercase letter or digit
    let last = name.chars().last().unwrap();
    if !last.is_ascii_lowercase() && !last.is_ascii_digit() {
        return Err(format!(
            "name must end with a lowercase letter or digit (got '{}')",
            last
        ));
    }

    // Middle characters must be lowercase letters, digits, or underscores
    for (i, c) in name.chars().enumerate() {
        if !c.is_ascii_lowercase() && !c.is_ascii_digit() && c != '_' {
            return Err(format!(
                "name must only contain lowercase letters, numbers, and underscores (got '{}' at position {})",
                c, i
            ));
        }
    }

    // Check reserved prefix
    if let Some(prefix) = reserved_prefix {
        if name.starts_with(prefix) {
            return Err(format!(
                "name must not start with reserved prefix '{}' (got '{}')",
                prefix, name
            ));
        }
    }

    Ok(())
}
