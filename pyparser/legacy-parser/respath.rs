//! Resource path parsing and validation.
//!
//! This module provides utilities for parsing and validating resource paths
//! used in Encore applications (e.g., API paths, resource names).

use std::fmt;

/// A parsed API path.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Path {
    /// The path segments.
    pub segments: Vec<Segment>,
}

/// A segment of an API path.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Segment {
    /// A literal path segment.
    Literal(String),
    /// A single-segment parameter (e.g., `:id`).
    Param(String),
    /// A wildcard parameter that matches multiple segments (e.g., `*path`).
    Wildcard(String),
}

impl Path {
    /// Parses a path string into a Path.
    pub fn parse(s: &str) -> Result<Self, PathError> {
        if s.is_empty() {
            return Err(PathError::Empty);
        }

        // Normalize: ensure starts with /
        let s = if s.starts_with('/') {
            s.to_string()
        } else {
            format!("/{}", s)
        };

        let mut segments = Vec::new();
        let mut has_wildcard = false;

        for part in s.split('/') {
            if part.is_empty() {
                continue;
            }

            if has_wildcard {
                return Err(PathError::SegmentAfterWildcard);
            }

            let segment = if let Some(name) = part.strip_prefix(':') {
                if name.is_empty() {
                    return Err(PathError::EmptyParam);
                }
                if !is_valid_param_name(name) {
                    return Err(PathError::InvalidParamName(name.to_string()));
                }
                Segment::Param(name.to_string())
            } else if let Some(name) = part.strip_prefix('*') {
                if name.is_empty() {
                    return Err(PathError::EmptyWildcard);
                }
                if !is_valid_param_name(name) {
                    return Err(PathError::InvalidParamName(name.to_string()));
                }
                has_wildcard = true;
                Segment::Wildcard(name.to_string())
            } else {
                if !is_valid_literal(part) {
                    return Err(PathError::InvalidLiteral(part.to_string()));
                }
                Segment::Literal(part.to_string())
            };

            segments.push(segment);
        }

        Ok(Path { segments })
    }

    /// Returns the path as a string.
    pub fn to_string(&self) -> String {
        if self.segments.is_empty() {
            return "/".to_string();
        }

        let parts: Vec<String> = self
            .segments
            .iter()
            .map(|s| match s {
                Segment::Literal(l) => l.clone(),
                Segment::Param(p) => format!(":{}", p),
                Segment::Wildcard(w) => format!("*{}", w),
            })
            .collect();

        format!("/{}", parts.join("/"))
    }

    /// Returns the parameter names in order.
    pub fn params(&self) -> Vec<&str> {
        self.segments
            .iter()
            .filter_map(|s| match s {
                Segment::Param(p) | Segment::Wildcard(p) => Some(p.as_str()),
                _ => None,
            })
            .collect()
    }

    /// Returns whether this path has a wildcard.
    pub fn has_wildcard(&self) -> bool {
        self.segments.iter().any(|s| matches!(s, Segment::Wildcard(_)))
    }
}

impl fmt::Display for Path {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_string())
    }
}

/// Errors that can occur when parsing a path.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PathError {
    /// The path is empty.
    Empty,
    /// A parameter name is empty (e.g., `/:`).
    EmptyParam,
    /// A wildcard name is empty (e.g., `/*`).
    EmptyWildcard,
    /// There's a segment after a wildcard.
    SegmentAfterWildcard,
    /// The parameter name is invalid.
    InvalidParamName(String),
    /// The literal segment is invalid.
    InvalidLiteral(String),
}

impl fmt::Display for PathError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            PathError::Empty => write!(f, "path cannot be empty"),
            PathError::EmptyParam => write!(f, "parameter name cannot be empty"),
            PathError::EmptyWildcard => write!(f, "wildcard name cannot be empty"),
            PathError::SegmentAfterWildcard => {
                write!(f, "cannot have path segments after a wildcard")
            }
            PathError::InvalidParamName(n) => write!(f, "invalid parameter name: {}", n),
            PathError::InvalidLiteral(l) => write!(f, "invalid path segment: {}", l),
        }
    }
}

impl std::error::Error for PathError {}

/// Checks if a parameter name is valid (alphanumeric + underscore, starts with letter).
fn is_valid_param_name(name: &str) -> bool {
    let mut chars = name.chars();
    match chars.next() {
        Some(c) if c.is_ascii_alphabetic() || c == '_' => {}
        _ => return false,
    }
    chars.all(|c| c.is_ascii_alphanumeric() || c == '_')
}

/// Checks if a literal segment is valid.
fn is_valid_literal(s: &str) -> bool {
    !s.is_empty()
        && s.chars()
            .all(|c| c.is_ascii_alphanumeric() || c == '-' || c == '_' || c == '.')
}

/// Validates a resource name.
pub fn validate_resource_name(name: &str) -> Result<(), String> {
    if name.is_empty() {
        return Err("resource name cannot be empty".to_string());
    }

    if name.len() > 63 {
        return Err("resource name cannot exceed 63 characters".to_string());
    }

    let mut chars = name.chars().peekable();

    // Must start with a letter
    match chars.next() {
        Some(c) if c.is_ascii_lowercase() => {}
        Some(c) => {
            return Err(format!(
                "resource name must start with a lowercase letter, got '{}'",
                c
            ))
        }
        None => return Err("resource name cannot be empty".to_string()),
    }

    // Must end with alphanumeric
    let last = name.chars().last().unwrap();
    if !last.is_ascii_alphanumeric() {
        return Err(format!(
            "resource name must end with an alphanumeric character, got '{}'",
            last
        ));
    }

    // Check all characters
    for c in name.chars() {
        if !c.is_ascii_lowercase() && !c.is_ascii_digit() && c != '-' {
            return Err(format!(
                "resource name can only contain lowercase letters, digits, and hyphens, got '{}'",
                c
            ));
        }
    }

    // No consecutive hyphens
    if name.contains("--") {
        return Err("resource name cannot contain consecutive hyphens".to_string());
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_path() {
        let path = Path::parse("/users/:id").unwrap();
        assert_eq!(path.segments.len(), 2);
        assert_eq!(path.segments[0], Segment::Literal("users".to_string()));
        assert_eq!(path.segments[1], Segment::Param("id".to_string()));
    }

    #[test]
    fn test_parse_wildcard() {
        let path = Path::parse("/files/*path").unwrap();
        assert_eq!(path.segments.len(), 2);
        assert!(path.has_wildcard());
        assert_eq!(path.params(), vec!["path"]);
    }

    #[test]
    fn test_segment_after_wildcard() {
        let result = Path::parse("/files/*path/more");
        assert!(matches!(result, Err(PathError::SegmentAfterWildcard)));
    }

    #[test]
    fn test_validate_resource_name() {
        assert!(validate_resource_name("mydb").is_ok());
        assert!(validate_resource_name("my-database").is_ok());
        assert!(validate_resource_name("db123").is_ok());

        assert!(validate_resource_name("").is_err());
        assert!(validate_resource_name("MyDB").is_err());
        assert!(validate_resource_name("-db").is_err());
        assert!(validate_resource_name("db-").is_err());
        assert!(validate_resource_name("db--test").is_err());
    }
}
