#![deny(clippy::all)]

use pyo3::prelude::*;

mod api;
mod asyncio;
mod error;
mod gateway;
mod log;
mod meta;
mod objects;
mod pubsub;
mod pvalue;
mod runtime;
mod runtime_config;
mod secret;
mod sqldb;

/// Encore runtime module for Python.
///
/// This module provides Python bindings for the Encore runtime,
/// enabling Python applications to use Encore's infrastructure primitives.
#[pymodule(gil_used = false)]
#[pyo3(name = "_runtime")]
fn encore_runtime(m: &Bound<'_, PyModule>) -> PyResult<()> {
    // Core runtime
    m.add_class::<runtime::Runtime>()?;

    // API types
    m.add_class::<api::Request>()?;
    m.add_class::<api::APIRoute>()?;

    // Infrastructure primitives
    m.add_class::<pubsub::PubSubTopic>()?;
    m.add_class::<pubsub::PubSubSubscription>()?;
    m.add_class::<pubsub::PubSubSubscriptionConfig>()?;
    m.add_class::<sqldb::SQLDatabase>()?;
    m.add_class::<sqldb::QueryArgs>()?;
    m.add_class::<sqldb::Cursor>()?;
    m.add_class::<sqldb::Row>()?;
    m.add_class::<sqldb::Transaction>()?;
    m.add_class::<objects::Bucket>()?;
    m.add_class::<objects::BucketObject>()?;
    m.add_class::<objects::ObjectAttrs>()?;
    m.add_class::<objects::ListIterator>()?;
    m.add_class::<gateway::Gateway>()?;
    m.add_class::<secret::Secret>()?;

    // Logging
    m.add_class::<log::Logger>()?;
    m.add_class::<log::LogLevel>()?;

    // Metadata
    m.add_class::<meta::AppMeta>()?;
    m.add_class::<meta::EnvironmentMeta>()?;
    m.add_class::<meta::EnvironmentType>()?;
    m.add_class::<meta::CloudProvider>()?;
    m.add_class::<meta::BuildMeta>()?;
    m.add_class::<meta::DeployMeta>()?;
    m.add_class::<meta::HostedService>()?;

    // Runtime config
    m.add_class::<runtime_config::RuntimeConfig>()?;
    m.add_class::<runtime_config::Metric>()?;

    // Error types
    m.add_class::<error::APIError>()?;

    // Version info
    m.add_function(wrap_pyfunction!(version, m)?)?;
    m.add_function(wrap_pyfunction!(build_commit, m)?)?;

    Ok(())
}

/// Returns the version of the Encore runtime.
#[pyfunction]
fn version() -> String {
    encore_runtime_core::version().to_string()
}

/// Returns the git commit hash used to build the Encore runtime.
#[pyfunction]
fn build_commit() -> String {
    encore_runtime_core::build_commit().to_string()
}
