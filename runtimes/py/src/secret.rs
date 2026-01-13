use pyo3::prelude::*;
use std::sync::Arc;

/// A secret value.
#[pyclass]
pub struct Secret {
    secret: Arc<encore_runtime_core::secrets::Secret>,
}

impl Secret {
    pub fn new(secret: Arc<encore_runtime_core::secrets::Secret>) -> Self {
        Self { secret }
    }
}

#[pymethods]
impl Secret {
    /// Get the cached value of the secret.
    ///
    /// Returns:
    ///     The secret value as a string.
    ///
    /// Raises:
    ///     RuntimeError: If the secret cannot be resolved.
    pub fn cached(&self) -> PyResult<String> {
        let val = self.secret.get().map_err(|e| {
            pyo3::exceptions::PyRuntimeError::new_err(format!("failed to resolve secret: {}", e))
        })?;
        String::from_utf8(val.to_vec())
            .map_err(|e| pyo3::exceptions::PyRuntimeError::new_err(e.to_string()))
    }

    fn __repr__(&self) -> String {
        "Secret(****)".to_string()
    }
}
