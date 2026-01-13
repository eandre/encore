use std::fmt;

/// A Python module name, represented as a period-separated string (e.g., "foo.bar.baz").
#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct ModulePath {
    name: String,
}

impl ModulePath {
    /// Creates a new ModulePath from a string, validating that it's a valid
    /// period-separated Python module name.
    ///
    /// Returns `None` if the name is invalid (empty, has empty segments,
    /// or contains invalid characters).
    pub fn new(name: impl Into<String>) -> Option<Self> {
        let name = name.into();
        if Self::is_valid(&name) {
            Some(ModulePath { name })
        } else {
            None
        }
    }

    /// Creates a new ModulePath from an iterator of segments.
    ///
    /// Returns `None` if the iterator is empty or any segment is invalid.
    pub fn from_segments<'a>(segments: impl IntoIterator<Item = &'a str>) -> Option<Self> {
        let mut name = String::new();
        for segment in segments {
            if !Self::is_valid_segment(segment) {
                return None;
            }
            if !name.is_empty() {
                name.push('.');
            }
            name.push_str(segment);
        }
        if name.is_empty() {
            None
        } else {
            Some(ModulePath { name })
        }
    }

    /// Checks if a string is a valid Python module name.
    fn is_valid(name: &str) -> bool {
        if name.is_empty() {
            return false;
        }

        for segment in name.split('.') {
            if !Self::is_valid_segment(segment) {
                return false;
            }
        }

        true
    }

    /// Checks if a segment is a valid Python identifier.
    fn is_valid_segment(segment: &str) -> bool {
        if segment.is_empty() {
            return false;
        }

        let mut chars = segment.chars();

        // First character must be a letter or underscore
        match chars.next() {
            Some(c) if c.is_ascii_alphabetic() || c == '_' => {}
            _ => return false,
        }

        // Remaining characters must be alphanumeric or underscore
        for c in chars {
            if !c.is_ascii_alphanumeric() && c != '_' {
                return false;
            }
        }

        true
    }

    /// Returns the module name as a string slice.
    pub fn as_str(&self) -> &str {
        &self.name
    }

    /// Consumes the ModuleName and returns the inner String.
    pub fn into_string(self) -> String {
        self.name
    }

    /// Returns an iterator over the segments of the module name.
    pub fn segments(&self) -> impl Iterator<Item = &str> {
        self.name.split('.')
    }

    /// Returns the number of segments in the module name.
    pub fn depth(&self) -> usize {
        self.name.split('.').count()
    }

    /// Returns the parent module name, or `None` if this is a top-level module.
    /// The parent is always a package since it must contain child modules.
    pub fn parent(&self) -> Option<ModulePath> {
        self.name.rsplit_once('.').map(|(parent, _)| ModulePath {
            name: parent.to_string(),
        })
    }

    /// Returns the last segment of the module name (the "leaf" name).
    pub fn leaf(&self) -> &str {
        self.name
            .rsplit_once('.')
            .map(|(_, leaf)| leaf)
            .unwrap_or(&self.name)
    }

    /// Joins this module path with a child segment.
    pub fn join(&self, child: &str) -> Option<ModulePath> {
        if !Self::is_valid_segment(child) {
            return None;
        }
        Some(ModulePath {
            name: format!("{}.{}", self.name, child),
        })
    }

    /// Resolves an import relative to the given package path.
    ///
    /// # Arguments
    /// * `pkg_path` - The package path of the module doing the import
    /// * `module` - The module being imported (e.g., "foo.bar" or empty for `from . import x`)
    /// * `level` - The relative import level (0 for absolute, 1 for `.`, 2 for `..`, etc.)
    ///
    /// # Returns
    /// The resolved module path, or `None` if the import is invalid.
    pub fn resolve_import(pkg_path: &ModulePath, module: Option<&str>, level: u32) -> Option<ModulePath> {
        if level == 0 {
            // Absolute import
            module.and_then(ModulePath::new)
        } else {
            // Relative import: go up (level - 1) levels from pkg_path
            let mut base = pkg_path.clone();
            for _ in 1..level {
                base = base.parent()?;
            }

            match module {
                Some(m) if !m.is_empty() => {
                    // Append the module path to the base
                    let combined = format!("{}.{}", base.name, m);
                    ModulePath::new(combined)
                }
                _ => Some(base),
            }
        }
    }
}

impl fmt::Display for ModulePath {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name)
    }
}

impl AsRef<str> for ModulePath {
    fn as_ref(&self) -> &str {
        &self.name
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_valid_module_names() {
        assert!(ModulePath::new("foo").is_some());
        assert!(ModulePath::new("foo.bar").is_some());
        assert!(ModulePath::new("foo.bar.baz").is_some());
        assert!(ModulePath::new("_private").is_some());
        assert!(ModulePath::new("foo_bar").is_some());
        assert!(ModulePath::new("foo123").is_some());
        assert!(ModulePath::new("_").is_some());
        assert!(ModulePath::new("__init__").is_some());
    }

    #[test]
    fn test_invalid_module_names() {
        assert!(ModulePath::new("").is_none());
        assert!(ModulePath::new(".").is_none());
        assert!(ModulePath::new("foo.").is_none());
        assert!(ModulePath::new(".foo").is_none());
        assert!(ModulePath::new("foo..bar").is_none());
        assert!(ModulePath::new("123foo").is_none());
        assert!(ModulePath::new("foo-bar").is_none());
        assert!(ModulePath::new("foo bar").is_none());
    }

    #[test]
    fn test_segments() {
        let name = ModulePath::new("foo.bar.baz").unwrap();
        let segments: Vec<_> = name.segments().collect();
        assert_eq!(segments, vec!["foo", "bar", "baz"]);
    }

    #[test]
    fn test_depth() {
        assert_eq!(ModulePath::new("foo").unwrap().depth(), 1);
        assert_eq!(ModulePath::new("foo.bar").unwrap().depth(), 2);
        assert_eq!(ModulePath::new("foo.bar.baz").unwrap().depth(), 3);
    }

    #[test]
    fn test_parent() {
        assert!(ModulePath::new("foo").unwrap().parent().is_none());

        let parent = ModulePath::new("foo.bar").unwrap().parent().unwrap();
        assert_eq!(parent.as_str(), "foo");

        let parent = ModulePath::new("foo.bar.baz").unwrap().parent().unwrap();
        assert_eq!(parent.as_str(), "foo.bar");
    }

    #[test]
    fn test_leaf() {
        assert_eq!(ModulePath::new("foo").unwrap().leaf(), "foo");
        assert_eq!(ModulePath::new("foo.bar").unwrap().leaf(), "bar");
        assert_eq!(ModulePath::new("foo.bar.baz").unwrap().leaf(), "baz");
    }

    #[test]
    fn test_from_segments() {
        assert_eq!(
            ModulePath::from_segments(["foo"]).unwrap().as_str(),
            "foo"
        );
        assert_eq!(
            ModulePath::from_segments(["foo", "bar"]).unwrap().as_str(),
            "foo.bar"
        );
        assert_eq!(
            ModulePath::from_segments(["foo", "bar", "baz"]).unwrap().as_str(),
            "foo.bar.baz"
        );

        // Empty iterator
        assert!(ModulePath::from_segments(std::iter::empty::<&str>()).is_none());

        // Invalid segment
        assert!(ModulePath::from_segments(["foo", "123invalid"]).is_none());
        assert!(ModulePath::from_segments(["foo", ""]).is_none());
    }
}
