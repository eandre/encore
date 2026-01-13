use encore_runtime_core::api;
use pyo3::prelude::*;

/// An error returned from an API call.
#[pyclass]
pub struct APIError {
    #[pyo3(get)]
    pub code: String,
    #[pyo3(get)]
    pub message: String,
    #[pyo3(get)]
    pub details: Option<Py<PyAny>>,
}

#[pymethods]
impl APIError {
    #[new]
    pub fn new(code: String, message: String, details: Option<Py<PyAny>>) -> Self {
        Self {
            code,
            message,
            details,
        }
    }

    fn __repr__(&self) -> String {
        format!("APIError(code={:?}, message={:?})", self.code, self.message)
    }

    fn __str__(&self) -> String {
        format!("{}: {}", self.code, self.message)
    }
}

impl From<api::Error> for APIError {
    fn from(err: api::Error) -> Self {
        Self {
            code: err.code.to_string(),
            message: err.message,
            details: None, // TODO: convert details to Python object
        }
    }
}

/// Helper to convert anyhow::Error to PyErr
pub fn to_py_err(err: anyhow::Error) -> PyErr {
    pyo3::exceptions::PyRuntimeError::new_err(format!("{:#}", err))
}

/// Helper to convert a generic error with Display to PyErr
#[allow(dead_code)]
pub fn to_py_err_display<E: std::fmt::Display>(err: E) -> PyErr {
    pyo3::exceptions::PyRuntimeError::new_err(err.to_string())
}

/// Helper to convert an API error to PyErr
pub fn api_err_to_py_err(err: api::Error) -> PyErr {
    pyo3::exceptions::PyRuntimeError::new_err(format!("{}: {}", err.code, err.message))
}
