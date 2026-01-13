//! File management and source location tracking.
//!
//! This module provides abstractions for managing source files and tracking
//! source locations within them.

use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicU32, Ordering};
use std::sync::{Arc, RwLock};

use anyhow::{Context, Result};
use ruff_text_size::{TextRange, TextSize};

/// A unique identifier for a file in the file set.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FileId(pub u32);

impl FileId {
    /// Creates a new FileId.
    pub fn new(id: u32) -> Self {
        FileId(id)
    }

    /// Returns the raw file ID value.
    pub fn raw(&self) -> u32 {
        self.0
    }
}

/// Represents a position within a source file.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub struct Pos(pub u32);

impl Pos {
    /// Creates a new position from a byte offset.
    pub fn new(offset: u32) -> Self {
        Pos(offset)
    }

    /// Creates a position from a TextSize.
    pub fn from_text_size(size: TextSize) -> Self {
        Pos(size.into())
    }

    /// Converts this position to a TextSize.
    pub fn to_text_size(self) -> TextSize {
        TextSize::new(self.0)
    }

    /// Returns whether this is a valid position.
    pub fn is_valid(&self) -> bool {
        self.0 > 0
    }
}

/// Represents a range within a source file.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub struct Range {
    /// The file this range belongs to.
    pub file_id: Option<FileId>,
    /// The start position (byte offset).
    pub start: Pos,
    /// The end position (byte offset).
    pub end: Pos,
}

impl Range {
    /// Creates a new range from start and end positions.
    pub fn new(file_id: Option<FileId>, start: Pos, end: Pos) -> Self {
        Range { file_id, start, end }
    }

    /// Creates a range from a TextRange and file ID.
    pub fn from_text_range(file_id: FileId, range: TextRange) -> Self {
        Range {
            file_id: Some(file_id),
            start: Pos::from_text_size(range.start()),
            end: Pos::from_text_size(range.end()),
        }
    }

    /// Creates an empty range.
    pub fn empty() -> Self {
        Range {
            file_id: None,
            start: Pos(0),
            end: Pos(0),
        }
    }

    /// Returns whether this range is valid.
    pub fn is_valid(&self) -> bool {
        self.file_id.is_some() && self.start.is_valid()
    }

    /// Converts this range to a TextRange.
    pub fn to_text_range(&self) -> TextRange {
        TextRange::new(self.start.to_text_size(), self.end.to_text_size())
    }

    /// Returns the length of this range in bytes.
    pub fn len(&self) -> u32 {
        self.end.0.saturating_sub(self.start.0)
    }

    /// Returns whether this range is empty.
    pub fn is_empty(&self) -> bool {
        self.start.0 >= self.end.0
    }

    /// Extends this range to cover another range.
    pub fn cover(self, other: Range) -> Range {
        if !self.is_valid() {
            return other;
        }
        if !other.is_valid() {
            return self;
        }
        Range {
            file_id: self.file_id,
            start: Pos(self.start.0.min(other.start.0)),
            end: Pos(self.end.0.max(other.end.0)),
        }
    }
}

/// Represents a path to a file, either real or synthetic.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum FilePath {
    /// A real filesystem path.
    Real(PathBuf),
    /// A synthetic path (e.g., for generated code).
    Custom(String),
}

impl FilePath {
    /// Returns the path as a string.
    pub fn as_str(&self) -> &str {
        match self {
            FilePath::Real(path) => path.to_str().unwrap_or(""),
            FilePath::Custom(s) => s,
        }
    }

    /// Returns whether this is a real filesystem path.
    pub fn is_real(&self) -> bool {
        matches!(self, FilePath::Real(_))
    }

    /// Returns the real path, if this is a real file.
    pub fn as_real(&self) -> Option<&Path> {
        match self {
            FilePath::Real(path) => Some(path),
            FilePath::Custom(_) => None,
        }
    }

    /// Returns the parent directory.
    pub fn parent(&self) -> Option<FilePath> {
        match self {
            FilePath::Real(path) => path.parent().map(|p| FilePath::Real(p.to_path_buf())),
            FilePath::Custom(_) => None,
        }
    }

    /// Returns the file name.
    pub fn file_name(&self) -> Option<&str> {
        match self {
            FilePath::Real(path) => path.file_name().and_then(|s| s.to_str()),
            FilePath::Custom(s) => s.rsplit('/').next(),
        }
    }
}

impl From<PathBuf> for FilePath {
    fn from(path: PathBuf) -> Self {
        FilePath::Real(path)
    }
}

impl From<&Path> for FilePath {
    fn from(path: &Path) -> Self {
        FilePath::Real(path.to_path_buf())
    }
}

impl std::fmt::Display for FilePath {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            FilePath::Real(path) => write!(f, "{}", path.display()),
            FilePath::Custom(s) => write!(f, "{}", s),
        }
    }
}

/// Line and column location within a file.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Loc {
    /// The file ID.
    pub file_id: FileId,
    /// Line number (1-based).
    pub line: u32,
    /// Column number (1-based).
    pub column: u32,
}

/// Information about a source file.
#[derive(Debug)]
pub struct SourceFile {
    /// The unique file ID.
    pub id: FileId,
    /// The file path.
    pub path: FilePath,
    /// The file content.
    pub content: String,
    /// Byte offsets of line starts.
    line_starts: Vec<u32>,
}

impl SourceFile {
    /// Creates a new source file.
    fn new(id: FileId, path: FilePath, content: String) -> Self {
        let line_starts = Self::compute_line_starts(&content);
        SourceFile {
            id,
            path,
            content,
            line_starts,
        }
    }

    /// Computes the byte offsets of line starts.
    fn compute_line_starts(content: &str) -> Vec<u32> {
        let mut starts = vec![0];
        for (i, c) in content.char_indices() {
            if c == '\n' {
                starts.push((i + 1) as u32);
            }
        }
        starts
    }

    /// Returns the line and column for a byte offset.
    pub fn line_col(&self, offset: u32) -> (u32, u32) {
        let line = self
            .line_starts
            .partition_point(|&start| start <= offset)
            .saturating_sub(1);
        let line_start = self.line_starts[line];
        let column = offset.saturating_sub(line_start);
        ((line + 1) as u32, column + 1)
    }

    /// Returns the content of a specific line (0-based).
    pub fn line_content(&self, line: usize) -> Option<&str> {
        if line >= self.line_starts.len() {
            return None;
        }
        let start = self.line_starts[line] as usize;
        let end = self
            .line_starts
            .get(line + 1)
            .map(|&e| e as usize)
            .unwrap_or(self.content.len());
        Some(&self.content[start..end])
    }

    /// Extracts text at the given range.
    pub fn text_at(&self, range: TextRange) -> &str {
        let start = usize::from(range.start());
        let end = usize::from(range.end());
        &self.content[start..end]
    }
}

/// A collection of source files.
pub struct FileSet {
    /// Counter for generating unique file IDs.
    next_id: AtomicU32,
    /// Map from file ID to source file.
    files: RwLock<HashMap<FileId, Arc<SourceFile>>>,
    /// Map from file path to file ID.
    path_to_id: RwLock<HashMap<FilePath, FileId>>,
}

impl FileSet {
    /// Creates a new empty file set.
    pub fn new() -> Self {
        FileSet {
            next_id: AtomicU32::new(1),
            files: RwLock::new(HashMap::new()),
            path_to_id: RwLock::new(HashMap::new()),
        }
    }

    /// Loads a file from the filesystem.
    pub fn load_file(&self, path: &Path) -> Result<Arc<SourceFile>> {
        let file_path = FilePath::Real(path.to_path_buf());

        // Check if already loaded
        {
            let path_to_id = self.path_to_id.read().unwrap();
            if let Some(&id) = path_to_id.get(&file_path) {
                let files = self.files.read().unwrap();
                if let Some(file) = files.get(&id) {
                    return Ok(Arc::clone(file));
                }
            }
        }

        // Load from disk
        let content = std::fs::read_to_string(path)
            .with_context(|| format!("failed to read file: {}", path.display()))?;

        self.add_file(file_path, content)
    }

    /// Adds a file with the given content.
    pub fn add_file(&self, path: FilePath, content: String) -> Result<Arc<SourceFile>> {
        // Check if already loaded
        {
            let path_to_id = self.path_to_id.read().unwrap();
            if let Some(&id) = path_to_id.get(&path) {
                let files = self.files.read().unwrap();
                if let Some(file) = files.get(&id) {
                    return Ok(Arc::clone(file));
                }
            }
        }

        // Create new file
        let id = FileId(self.next_id.fetch_add(1, Ordering::SeqCst));
        let file = Arc::new(SourceFile::new(id, path.clone(), content));

        // Store
        {
            let mut files = self.files.write().unwrap();
            let mut path_to_id = self.path_to_id.write().unwrap();
            files.insert(id, Arc::clone(&file));
            path_to_id.insert(path, id);
        }

        Ok(file)
    }

    /// Creates a synthetic source file (e.g., for generated code).
    pub fn new_source_file(&self, name: String, content: String) -> Result<Arc<SourceFile>> {
        self.add_file(FilePath::Custom(name), content)
    }

    /// Looks up a file by ID.
    pub fn lookup_file(&self, id: FileId) -> Option<Arc<SourceFile>> {
        let files = self.files.read().unwrap();
        files.get(&id).cloned()
    }

    /// Looks up a file by path.
    pub fn lookup_by_path(&self, path: &FilePath) -> Option<Arc<SourceFile>> {
        let path_to_id = self.path_to_id.read().unwrap();
        let id = path_to_id.get(path)?;
        let files = self.files.read().unwrap();
        files.get(id).cloned()
    }

    /// Returns the location (file, line, column) for a range.
    pub fn loc(&self, range: &Range) -> Option<Loc> {
        let file_id = range.file_id?;
        let file = self.lookup_file(file_id)?;
        let (line, column) = file.line_col(range.start.0);
        Some(Loc {
            file_id,
            line,
            column,
        })
    }

    /// Returns the file path for a file ID.
    pub fn file_path(&self, id: FileId) -> Option<FilePath> {
        let files = self.files.read().unwrap();
        files.get(&id).map(|f| f.path.clone())
    }
}

impl Default for FileSet {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_line_col() {
        let content = "line1\nline2\nline3\n".to_string();
        let file = SourceFile::new(FileId(1), FilePath::Custom("test".into()), content);

        assert_eq!(file.line_col(0), (1, 1)); // start of line1
        assert_eq!(file.line_col(5), (1, 6)); // newline after line1
        assert_eq!(file.line_col(6), (2, 1)); // start of line2
        assert_eq!(file.line_col(11), (2, 6)); // newline after line2
        assert_eq!(file.line_col(12), (3, 1)); // start of line3
    }

    #[test]
    fn test_range_cover() {
        let r1 = Range {
            file_id: Some(FileId(1)),
            start: Pos(10),
            end: Pos(20),
        };
        let r2 = Range {
            file_id: Some(FileId(1)),
            start: Pos(5),
            end: Pos(15),
        };
        let covered = r1.cover(r2);
        assert_eq!(covered.start.0, 5);
        assert_eq!(covered.end.0, 20);
    }
}
