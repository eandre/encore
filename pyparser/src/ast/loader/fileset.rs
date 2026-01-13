//! File management and source location tracking.
//!
//! This module provides abstractions for managing source files and tracking
//! source locations within them.

use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::sync::{Arc, OnceLock, RwLock};

use ruff_source_file::LineIndex;
use ruff_text_size::TextSize;

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

/// A unique identifier for a file in the file set.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct FileId(u32);

impl FileId {
    /// Creates a new FileId.
    pub fn new(id: u32) -> Self {
        FileId(id)
    }

    /// Returns the raw file ID value.
    pub fn raw(&self) -> u32 {
        self.0
    }

    pub fn to_pos(&self, size: TextSize) -> Pos {
        Pos::new(*self, size.into())
    }

    pub fn to_span<R: ruff_text_size::Ranged>(&self, ranged: R) -> Span {
        let r = ranged.range();
        Span::new(*self, r.start().into(), r.end().into())
    }
}

/// Represents a position within a source file.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub struct Pos(FileId, u32);

impl Pos {
    /// Creates a new position from a byte offset.
    pub fn new(file: FileId, offset: u32) -> Self {
        Pos(file, offset)
    }

    /// Returns whether this is a valid position.
    pub fn is_valid(&self) -> bool {
        self.0 > FileId(0u32)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub struct Span(FileId, u32, u32);

impl Span {
    pub fn new(file: FileId, start: u32, end: u32) -> Self {
        Self(file, start, end)
    }

    /// Returns whether this is a valid position.
    pub fn is_valid(&self) -> bool {
        self.0 > FileId(0u32)
    }

    pub fn file_id(&self) -> FileId {
        self.0
    }

    pub fn start(&self) -> u32 {
        self.1
    }

    pub fn end(&self) -> u32 {
        self.2
    }
}
pub trait Spanned {
    fn span(&self) -> Span;
}

impl Spanned for Span {
    fn span(&self) -> Span {
        *self
    }
}

/// A cached file entry containing file metadata and contents.
#[derive(Debug)]
pub struct File {
    pub id: FileId,
    pub path: FilePath,
    pub contents: String,
    line_index: OnceLock<LineIndex>,
}

impl File {
    /// Creates a new File for testing purposes.
    #[cfg(test)]
    pub fn new_for_test(id: FileId, path: FilePath, contents: String) -> Self {
        File {
            id,
            path,
            contents,
            line_index: OnceLock::new(),
        }
    }

    /// Returns the line index for this file, lazily initializing it if needed.
    pub fn line_index(&self) -> &LineIndex {
        self.line_index
            .get_or_init(|| LineIndex::from_source_text(&self.contents))
    }
}

/// A collection of source files with caching.
///
/// FileSet loads files from disk and caches them in memory,
/// ensuring each file is only loaded once and assigned a unique FileId.
pub struct FileSet {
    files_by_path: RwLock<HashMap<FilePath, Arc<File>>>,
    files_by_id: RwLock<Vec<Arc<File>>>,
}

impl std::fmt::Debug for FileSet {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let num_files = self.files_by_id.read().unwrap().len();
        f.debug_struct("FileSet")
            .field("num_files", &num_files)
            .finish_non_exhaustive()
    }
}

impl FileSet {
    /// Creates a new empty FileSet.
    pub fn new() -> Self {
        FileSet {
            files_by_path: RwLock::new(HashMap::new()),
            files_by_id: RwLock::new(Vec::new()),
        }
    }

    /// Gets or loads a file by its path.
    ///
    /// If the file has already been loaded, returns the cached version.
    /// Otherwise, loads the file from disk, caches it, and returns it.
    pub fn get(&self, path: &FilePath) -> std::io::Result<Arc<File>> {
        // Check if already cached (fast path with read lock)
        if let Some(file) = self.files_by_path.read().unwrap().get(path) {
            return Ok(Arc::clone(file));
        }

        // Load from disk before acquiring write lock
        let contents = match path {
            FilePath::Real(p) => std::fs::read_to_string(p)?,
            FilePath::Custom(_) => {
                return Err(std::io::Error::new(
                    std::io::ErrorKind::NotFound,
                    "cannot load custom file path from disk",
                ));
            }
        };

        // Acquire write locks
        let mut files_by_path = self.files_by_path.write().unwrap();
        let mut files_by_id = self.files_by_id.write().unwrap();

        // Double-check after acquiring write lock
        if let Some(file) = files_by_path.get(path) {
            return Ok(Arc::clone(file));
        }

        let id = FileId::new((files_by_id.len() + 1) as u32);
        let file = Arc::new(File {
            id,
            path: path.clone(),
            contents,
            line_index: OnceLock::new(),
        });
        files_by_id.push(Arc::clone(&file));
        files_by_path.insert(path.clone(), Arc::clone(&file));

        Ok(file)
    }

    /// Looks up a file by its FileId.
    pub fn get_by_id(&self, id: FileId) -> Arc<File> {
        let idx = id.raw() as usize;
        if idx == 0 {
            panic!("invalid zero FileId");
        }

        Arc::clone(
            self.files_by_id
                .read()
                .unwrap()
                .get(idx - 1)
                .expect("file not found"),
        )
    }

    /// Inserts a file with custom contents (e.g., for synthetic files).
    pub fn insert(&self, path: FilePath, contents: String) -> Arc<File> {
        // Acquire write locks
        let mut files_by_path = self.files_by_path.write().unwrap();
        let mut files_by_id = self.files_by_id.write().unwrap();

        // Return existing if already present
        if let Some(file) = files_by_path.get(&path) {
            return Arc::clone(file);
        }

        let id = FileId::new((files_by_id.len() + 1) as u32);
        let file = Arc::new(File {
            id,
            path: path.clone(),
            contents,
            line_index: OnceLock::new(),
        });
        files_by_id.push(Arc::clone(&file));
        files_by_path.insert(path, Arc::clone(&file));

        file
    }
}

impl Default for FileSet {
    fn default() -> Self {
        Self::new()
    }
}
