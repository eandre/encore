use thiserror::Error;

use crate::ast::loader::{
    fileset::{FilePath, Span, Spanned},
    module_resolver::ResolveError,
};

#[derive(Debug, Clone, Error)]
pub enum ParseError {
    #[error("parse error at {span:?}: {message}")]
    Parse { span: Span, message: String },

    #[error("resolve error: {0}")]
    Resolve(#[from] ResolveError),

    #[error("unable to read {file_path:?}: {message}")]
    IO {
        file_path: Option<FilePath>,
        message: String,
    },
}

pub type ParseResult<T> = Result<T, ParseError>;

pub trait ErrorReporter {
    fn err(&self, msg: &str) -> ParseError;
}

impl<S: Spanned> ErrorReporter for S {
    fn err(&self, msg: &str) -> ParseError {
        ParseError::Parse {
            span: self.span(),
            message: msg.to_string(),
        }
    }
}
