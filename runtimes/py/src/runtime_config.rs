use pyo3::prelude::*;
use std::collections::HashMap;

/// A metric configuration.
#[pyclass]
#[derive(Clone)]
pub struct Metric {
    #[pyo3(get)]
    pub name: String,
    #[pyo3(get)]
    pub services: Vec<String>,
}

impl From<encore_runtime_core::runtime_config::Metric> for Metric {
    fn from(metric: encore_runtime_core::runtime_config::Metric) -> Self {
        Self {
            name: metric.name,
            services: metric.services,
        }
    }
}

#[pymethods]
impl Metric {
    fn __repr__(&self) -> String {
        format!("Metric(name={:?})", self.name)
    }
}

/// Runtime configuration.
#[pyclass]
#[derive(Clone)]
pub struct RuntimeConfig {
    #[pyo3(get)]
    pub metrics: HashMap<String, Metric>,
}

impl From<encore_runtime_core::runtime_config::RuntimeConfig> for RuntimeConfig {
    fn from(config: encore_runtime_core::runtime_config::RuntimeConfig) -> Self {
        Self {
            metrics: config
                .metrics
                .into_iter()
                .map(|(k, v)| (k, v.into()))
                .collect(),
        }
    }
}

#[pymethods]
impl RuntimeConfig {
    fn __repr__(&self) -> String {
        format!("RuntimeConfig(metrics={})", self.metrics.len())
    }
}
