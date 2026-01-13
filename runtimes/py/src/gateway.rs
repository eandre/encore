use encore_runtime_core::api::gateway as core_gw;
use pyo3::prelude::*;
use std::sync::Arc;

/// Gateway configuration.
#[pyclass]
pub struct GatewayConfig {
    #[pyo3(get, set)]
    pub auth_handler: Option<Py<PyAny>>,
}

#[pymethods]
impl GatewayConfig {
    #[new]
    #[pyo3(signature = (auth_handler=None))]
    pub fn new(auth_handler: Option<Py<PyAny>>) -> Self {
        Self { auth_handler }
    }

    fn __repr__(&self) -> String {
        "GatewayConfig()".to_string()
    }
}

/// An auth gateway.
#[pyclass]
pub struct Gateway {
    #[allow(dead_code)]
    gw: Option<Arc<core_gw::Gateway>>,
    #[allow(dead_code)]
    config: GatewayConfig,
}

impl Gateway {
    pub fn new(gw: Option<Arc<core_gw::Gateway>>, config: &GatewayConfig) -> PyResult<Self> {
        // TODO: Register auth handler with the gateway
        // For now, we just store the config
        // We need to clone the handler from config using Python GIL
        let config = Python::attach(|py| GatewayConfig {
            auth_handler: config.auth_handler.as_ref().map(|h| h.clone_ref(py)),
        });

        Ok(Self { gw, config })
    }
}

#[pymethods]
impl Gateway {
    fn __repr__(&self) -> String {
        "Gateway()".to_string()
    }
}
