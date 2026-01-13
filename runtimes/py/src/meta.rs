use encore_runtime_core::meta;
use pyo3::prelude::*;
use std::collections::HashMap;

/// Application metadata.
#[pyclass]
#[derive(Clone)]
pub struct AppMeta {
    #[pyo3(get)]
    pub app_id: String,
    #[pyo3(get)]
    pub api_base_url: String,
    #[pyo3(get)]
    pub environment: EnvironmentMeta,
    #[pyo3(get)]
    pub build: BuildMeta,
    #[pyo3(get)]
    pub deploy: DeployMeta,
}

impl From<meta::AppMeta> for AppMeta {
    fn from(rt: meta::AppMeta) -> Self {
        AppMeta {
            app_id: rt.app_id,
            api_base_url: rt.api_base_url,
            environment: rt.environment.into(),
            build: rt.build.into(),
            deploy: rt.deploy.into(),
        }
    }
}

#[pymethods]
impl AppMeta {
    fn __repr__(&self) -> String {
        format!(
            "AppMeta(app_id={:?}, environment={:?})",
            self.app_id, self.environment.name
        )
    }
}

/// Environment metadata.
#[pyclass]
#[derive(Clone)]
pub struct EnvironmentMeta {
    #[pyo3(get)]
    pub name: String,
    #[pyo3(get)]
    pub env_type: EnvironmentType,
    #[pyo3(get)]
    pub cloud: CloudProvider,
}

impl From<meta::EnvironmentMeta> for EnvironmentMeta {
    fn from(rt: meta::EnvironmentMeta) -> Self {
        EnvironmentMeta {
            name: rt.name,
            env_type: rt.r#type.into(),
            cloud: rt.cloud.into(),
        }
    }
}

#[pymethods]
impl EnvironmentMeta {
    fn __repr__(&self) -> String {
        format!(
            "EnvironmentMeta(name={:?}, type={:?})",
            self.name, self.env_type
        )
    }
}

/// Environment type enum.
#[pyclass(eq, eq_int)]
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum EnvironmentType {
    Production,
    Development,
    Ephemeral,
    Test,
}

impl From<meta::EnvironmentType> for EnvironmentType {
    fn from(rt: meta::EnvironmentType) -> Self {
        match rt {
            meta::EnvironmentType::Production => Self::Production,
            meta::EnvironmentType::Development => Self::Development,
            meta::EnvironmentType::Ephemeral => Self::Ephemeral,
            meta::EnvironmentType::Test => Self::Test,
        }
    }
}

/// Cloud provider enum.
#[pyclass(eq, eq_int)]
#[derive(Clone, Copy, Debug, PartialEq)]
#[allow(clippy::upper_case_acronyms)]
pub enum CloudProvider {
    AWS,
    GCP,
    Azure,
    Encore,
    Local,
}

impl From<meta::CloudProvider> for CloudProvider {
    fn from(rt: meta::CloudProvider) -> Self {
        match rt {
            meta::CloudProvider::AWS => Self::AWS,
            meta::CloudProvider::GCP => Self::GCP,
            meta::CloudProvider::Azure => Self::Azure,
            meta::CloudProvider::Encore => Self::Encore,
            meta::CloudProvider::Local => Self::Local,
        }
    }
}

/// Build metadata.
#[pyclass]
#[derive(Clone)]
pub struct BuildMeta {
    #[pyo3(get)]
    pub revision: String,
    #[pyo3(get)]
    pub uncommitted_changes: bool,
}

impl From<meta::BuildMeta> for BuildMeta {
    fn from(rt: meta::BuildMeta) -> Self {
        BuildMeta {
            revision: rt.revision,
            uncommitted_changes: rt.uncommitted_changes,
        }
    }
}

#[pymethods]
impl BuildMeta {
    fn __repr__(&self) -> String {
        format!("BuildMeta(revision={:?})", self.revision)
    }
}

/// Hosted service info.
#[pyclass]
#[derive(Clone)]
pub struct HostedService {
    #[pyo3(get)]
    pub name: String,
}

impl From<meta::HostedService> for HostedService {
    fn from(rt: meta::HostedService) -> Self {
        HostedService { name: rt.name }
    }
}

#[pymethods]
impl HostedService {
    fn __repr__(&self) -> String {
        format!("HostedService(name={:?})", self.name)
    }
}

/// Deployment metadata.
#[pyclass]
#[derive(Clone)]
pub struct DeployMeta {
    #[pyo3(get)]
    pub id: String,
    #[pyo3(get)]
    pub deploy_time: String,
    #[pyo3(get)]
    pub hosted_services: HashMap<String, HostedService>,
}

impl From<meta::DeployMeta> for DeployMeta {
    fn from(rt: meta::DeployMeta) -> Self {
        DeployMeta {
            id: rt.id,
            deploy_time: rt.deploy_time.to_string(),
            hosted_services: rt
                .hosted_services
                .into_iter()
                .map(|svc| (svc.name.clone(), svc.into()))
                .collect(),
        }
    }
}

#[pymethods]
impl DeployMeta {
    fn __repr__(&self) -> String {
        format!("DeployMeta(id={:?})", self.id)
    }
}
