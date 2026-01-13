//! Documentation comment extraction for Python.
//!
//! This module provides utilities for extracting documentation from
//! Python docstrings and comments.

use ruff_python_ast as ast;

/// Extracts the docstring from a function definition.
pub fn extract_docstring(func: &ast::StmtFunctionDef) -> Option<String> {
    // The docstring is the first statement if it's a string expression
    if let Some(first_stmt) = func.body.first() {
        if let ast::Stmt::Expr(expr_stmt) = first_stmt {
            if let ast::Expr::StringLiteral(lit) = expr_stmt.value.as_ref() {
                return Some(clean_docstring(&lit.value.to_string()));
            }
        }
    }
    None
}

/// Extracts the docstring from a class definition.
pub fn extract_class_docstring(class: &ast::StmtClassDef) -> Option<String> {
    // The docstring is the first statement if it's a string expression
    if let Some(first_stmt) = class.body.first() {
        if let ast::Stmt::Expr(expr_stmt) = first_stmt {
            if let ast::Expr::StringLiteral(lit) = expr_stmt.value.as_ref() {
                return Some(clean_docstring(&lit.value.to_string()));
            }
        }
    }
    None
}

/// Extracts the docstring from a module.
pub fn extract_module_docstring(module: &ast::ModModule) -> Option<String> {
    // The docstring is the first statement if it's a string expression
    if let Some(first_stmt) = module.body.first() {
        if let ast::Stmt::Expr(expr_stmt) = first_stmt {
            if let ast::Expr::StringLiteral(lit) = expr_stmt.value.as_ref() {
                return Some(clean_docstring(&lit.value.to_string()));
            }
        }
    }
    None
}

/// Cleans a docstring by removing leading/trailing whitespace and normalizing indentation.
fn clean_docstring(s: &str) -> String {
    let lines: Vec<&str> = s.lines().collect();

    if lines.is_empty() {
        return String::new();
    }

    // Find minimum indentation (ignoring empty lines and first line)
    let min_indent = lines
        .iter()
        .skip(1)
        .filter(|line| !line.trim().is_empty())
        .map(|line| line.len() - line.trim_start().len())
        .min()
        .unwrap_or(0);

    // Remove the common indentation from all lines except the first
    let mut result: Vec<String> = Vec::new();
    for (i, line) in lines.iter().enumerate() {
        if i == 0 {
            result.push(line.trim().to_string());
        } else if line.trim().is_empty() {
            result.push(String::new());
        } else if line.len() >= min_indent {
            result.push(line[min_indent..].to_string());
        } else {
            result.push(line.to_string());
        }
    }

    // Trim leading and trailing empty lines
    while result.first().map(|s| s.is_empty()).unwrap_or(false) {
        result.remove(0);
    }
    while result.last().map(|s| s.is_empty()).unwrap_or(false) {
        result.pop();
    }

    result.join("\n")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_clean_docstring() {
        let docstring = r#"
        This is a docstring.

        With multiple lines.
        And some indentation.
        "#;

        let cleaned = clean_docstring(docstring);
        assert!(cleaned.starts_with("This is a docstring."));
        assert!(cleaned.contains("With multiple lines."));
    }

    #[test]
    fn test_clean_single_line() {
        let docstring = "Simple docstring";
        let cleaned = clean_docstring(docstring);
        assert_eq!(cleaned, "Simple docstring");
    }
}
