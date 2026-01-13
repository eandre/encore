use crate::api::{APIRoute, Request, new_api_handler};
use crate::error::{api_err_to_py_err, to_py_err};
use crate::gateway::{Gateway, GatewayConfig};
use crate::log::Logger;
use crate::meta::AppMeta;
use crate::objects::Bucket;
use crate::pubsub::{PubSubSubscription, PubSubSubscriptionConfig, PubSubTopic};
use crate::pvalue::{pvalues_to_py, py_to_pvalues};
use crate::runtime_config::RuntimeConfig;
use crate::secret::Secret;
use crate::sqldb::SQLDatabase;
use encore_runtime_core::pubsub::SubName;
use encore_runtime_core::{EncoreName, api};
use pyo3::exceptions::PyKeyboardInterrupt;
use pyo3::prelude::*;
use std::sync::{Arc, OnceLock};
use std::thread;

/// Global runtime instance (singleton pattern for non-test mode).
/// We store Result<Arc<Runtime>, String> instead of PyResult because PyErr doesn't implement Clone.
static RUNTIME: OnceLock<Result<Arc<encore_runtime_core::Runtime>, String>> = OnceLock::new();

/// Initialize the Encore runtime.
fn init_runtime(test_mode: bool) -> PyResult<encore_runtime_core::Runtime> {
    // Initialize logging.
    encore_runtime_core::log::init();

    let tokio_rt = tokio::runtime::Builder::new_multi_thread()
        .enable_all()
        .build()
        .expect("unable to initialize tokio runtime");

    let tokio_rt: &'static tokio::runtime::Runtime = Box::leak(Box::new(tokio_rt));

    pyo3_async_runtimes::tokio::init_with_runtime(tokio_rt).expect("failed to init with runtime");

    let core = encore_runtime_core::Runtime::builder()
        .with_test_mode(test_mode)
        .with_meta_autodetect()
        .with_runtime_config_from_env()
        .with_tokio_handle(tokio_rt.handle().clone())
        .build()
        .map_err(to_py_err)?;

    Ok(core)
}

/// The main Encore runtime class.
///
/// This class provides access to all Encore infrastructure primitives
/// including API handlers, databases, pub/sub, object storage, and more.
#[pyclass]
pub struct Runtime {
    pub(crate) runtime: Arc<encore_runtime_core::Runtime>,
    pub(crate) event_loop: Py<PyAny>,
    pub(crate) task_locals: pyo3_async_runtimes::TaskLocals,
}

#[pymethods]
impl Runtime {
    /// Create a new Runtime instance.
    ///
    /// Args:
    ///     event_loop: The Python asyncio event loop to use for async handlers.
    ///     test_mode: If True, creates an isolated runtime for testing.
    ///                Each test gets its own runtime instance.
    #[new]
    #[pyo3(signature = (event_loop, test_mode=None))]
    pub fn new(py: Python<'_>, event_loop: Py<PyAny>, test_mode: Option<bool>) -> PyResult<Self> {
        let test_mode = test_mode.unwrap_or(false);
        let task_locals =
            pyo3_async_runtimes::TaskLocals::new(event_loop.bind(py).clone()).copy_context(py)?;

        if test_mode {
            // Don't reuse the runtime in tests, as test frameworks
            // may use isolation between tests.
            let runtime = Arc::new(init_runtime(true)?);

            // Start the runtime in the background immediately for tests.
            {
                let rt = runtime.clone();
                thread::spawn(move || {
                    rt.run_blocking();
                });
            }

            return Ok(Self {
                runtime,
                event_loop,
                task_locals,
            });
        }

        // For non-test mode, use the singleton runtime.
        let runtime_ref =
            RUNTIME.get_or_init(|| init_runtime(false).map(Arc::new).map_err(|e| e.to_string()));

        match runtime_ref {
            Ok(runtime) => Ok(Self {
                runtime: runtime.clone(),
                event_loop,
                task_locals,
            }),
            Err(e) => Err(pyo3::exceptions::PyRuntimeError::new_err(e.clone())),
        }
    }

    /// Run the runtime forever.
    ///
    /// This starts the runtime and blocks until shutdown.
    /// Uses the event loop provided during construction.
    pub fn run_forever(&self, py: Python<'_>) -> PyResult<()> {
        let rt = self.runtime.clone();
        let event_loop = self.event_loop.bind(py).clone();
        let result = pyo3_async_runtimes::tokio::run_until_complete(event_loop, async move {
            rt.run().await;
            Ok(())
        });

        // Handle keyboard interrupts gracefully
        match result {
            Err(e) if e.is_instance_of::<PyKeyboardInterrupt>(py) => Ok(()),
            other => other,
        }
    }

    /// Get a SQL database by its Encore name.
    ///
    /// Args:
    ///     encore_name: The name of the database as defined in Encore.
    ///
    /// Returns:
    ///     A SQLDatabase instance for executing queries.
    pub fn sql_database(&self, encore_name: String) -> SQLDatabase {
        let encore_name: EncoreName = encore_name.into();
        let db = self.runtime.sqldb().database(&encore_name);
        SQLDatabase::new(
            self.runtime.tokio_handle().clone(),
            db,
            self.task_locals.clone(),
        )
    }

    /// Get a PubSub topic by its Encore name.
    ///
    /// Args:
    ///     encore_name: The name of the topic as defined in Encore.
    ///
    /// Returns:
    ///     A PubSubTopic instance for publishing messages.
    pub fn pubsub_topic(&self, encore_name: String) -> PyResult<PubSubTopic> {
        let name_for_error = encore_name.clone();
        // Enter the Tokio runtime context before accessing the topic
        let _guard = self.runtime.tokio_handle().enter();
        let topic = self
            .runtime
            .pubsub()
            .topic(encore_name.into())
            .ok_or_else(|| {
                pyo3::exceptions::PyValueError::new_err(format!(
                    "topic '{}' not found",
                    name_for_error
                ))
            })?;
        Ok(PubSubTopic::new(topic, self.task_locals.clone()))
    }

    /// Get an object storage bucket by its Encore name.
    ///
    /// Args:
    ///     encore_name: The name of the bucket as defined in Encore.
    ///
    /// Returns:
    ///     A Bucket instance for object storage operations.
    pub fn bucket(&self, encore_name: String) -> PyResult<Bucket> {
        let bkt = self
            .runtime
            .objects()
            .bucket(encore_name.clone().into())
            .ok_or_else(|| {
                pyo3::exceptions::PyValueError::new_err(format!(
                    "bucket '{}' not found",
                    encore_name
                ))
            })?;
        Ok(Bucket::new(bkt))
    }

    /// Get an auth gateway by its Encore name.
    ///
    /// Args:
    ///     encore_name: The name of the gateway as defined in Encore.
    ///     config: Gateway configuration including the auth handler.
    ///
    /// Returns:
    ///     A Gateway instance.
    pub fn gateway(&self, encore_name: String, config: &GatewayConfig) -> PyResult<Gateway> {
        let name: EncoreName = encore_name.into();
        let gw = self.runtime.api().gateway(&name).cloned().map(Arc::new);
        Gateway::new(gw, config)
    }

    /// Get the root logger.
    ///
    /// Returns:
    ///     A Logger instance for structured logging.
    pub fn logger(&self) -> Logger {
        Logger::new()
    }

    /// Create a PubSub subscription.
    ///
    /// Args:
    ///     config: Subscription configuration including topic, name, and handler.
    ///
    /// Returns:
    ///     A PubSubSubscription instance.
    pub fn pubsub_subscription(
        &self,
        py: Python<'_>,
        config: &PubSubSubscriptionConfig,
    ) -> PyResult<PubSubSubscription> {
        let sub = self
            .runtime
            .pubsub()
            .subscription(SubName {
                topic: config.topic_name.clone().into(),
                subscription: config.subscription_name.clone().into(),
            })
            .ok_or_else(|| {
                pyo3::exceptions::PyValueError::new_err(format!(
                    "subscription '{}/{}' not found",
                    config.topic_name, config.subscription_name
                ))
            })?;

        let handler = config.handler.clone_ref(py);
        let task_locals = self.task_locals.clone();

        // Spawn a background task to process messages
        self.runtime.tokio_handle().spawn(async move {
            if let Err(e) = crate::pubsub::subscribe(sub, handler, task_locals).await {
                log::error!("subscription error: {}", e);
            }
        });

        Ok(PubSubSubscription::new())
    }

    /// Register an API handler.
    ///
    /// Args:
    ///     route: The API route configuration including service, name, and handler.
    pub fn register_handler(&self, py: Python<'_>, route: &APIRoute) -> PyResult<()> {
        let endpoint_name = encore_runtime_core::EndpointName::new(&route.service, &route.name);

        let eps = self.runtime.api().endpoints();
        let resp_schema = eps.get(&endpoint_name).map(|ep| ep.response.clone());

        let handler = new_api_handler(
            py,
            &route.handler,
            route.raw,
            route.streaming,
            resp_schema,
            self.task_locals.clone(),
        )?;

        // If we're not hosting an API server, this is a no-op.
        let Some(srv) = self.runtime.api().server() else {
            return Ok(());
        };

        srv.register_handler(endpoint_name, handler)
            .map_err(to_py_err)
    }

    /// Register multiple API handlers.
    ///
    /// Args:
    ///     routes: A list of API route configurations.
    pub fn register_handlers(
        &self,
        py: Python<'_>,
        routes: Vec<Bound<'_, APIRoute>>,
    ) -> PyResult<()> {
        for route in routes {
            self.register_handler(py, &route.borrow())?;
        }
        Ok(())
    }

    /// Get a secret by its Encore name.
    ///
    /// Args:
    ///     encore_name: The name of the secret as defined in Encore.
    ///
    /// Returns:
    ///     A Secret instance, or None if not found.
    pub fn secret(&self, encore_name: String) -> Option<Secret> {
        self.runtime
            .secrets()
            .app_secret(encore_name.into())
            .map(Secret::new)
    }

    /// Make an API call to another endpoint.
    ///
    /// Args:
    ///     service: The service name.
    ///     endpoint: The endpoint name.
    ///     payload: Optional request payload as a dict.
    ///     source: Optional source request for tracing.
    ///
    /// Returns:
    ///     The response payload, or raises an APIError on failure.
    #[pyo3(signature = (service, endpoint, payload=None, source=None))]
    pub fn api_call<'py>(
        &self,
        py: Python<'py>,
        service: String,
        endpoint: String,
        payload: Option<Bound<'py, pyo3::types::PyAny>>,
        source: Option<&Request>,
    ) -> PyResult<Py<PyAny>> {
        let endpoint_name = encore_runtime_core::EndpointName::new(service, endpoint);

        let payload = match payload {
            Some(ref p) => py_to_pvalues(p)?,
            None => None,
        };

        let source = source.map(|s| s.inner.clone());

        // Use pyo3's allow_threads to release the GIL during the async call
        let runtime = self.runtime.clone();
        let tokio_handle = runtime.tokio_handle().clone();
        let result = py.detach(move || {
            // Run the async call synchronously using tokio
            tokio_handle.block_on(async move {
                runtime
                    .api()
                    .call(endpoint_name, payload, source, None)
                    .await
            })
        });

        match result {
            Ok(data) => {
                let payload = match (data.header, data.body) {
                    (None, api::Body::Raw(_) | api::Body::Typed(None)) => None,
                    (None, api::Body::Typed(Some(body))) => Some(body),
                    (Some(header), api::Body::Raw(_) | api::Body::Typed(None)) => Some(header),
                    (Some(header), api::Body::Typed(Some(body))) => {
                        let mut combined = header;
                        combined.extend(body);
                        Some(combined)
                    }
                };
                pvalues_to_py(py, payload.as_ref())
            }
            Err(err) => Err(api_err_to_py_err(err)),
        }
    }

    /// Get the application metadata.
    ///
    /// Returns:
    ///     AppMeta containing app ID, environment info, build info, etc.
    pub fn app_meta(&self) -> AppMeta {
        let md = self.runtime.app_meta();
        md.clone().into()
    }

    /// Get the runtime configuration.
    ///
    /// Returns:
    ///     RuntimeConfig containing metrics configuration.
    pub fn runtime_config(&self) -> RuntimeConfig {
        let rt = self.runtime.runtime_config();
        rt.clone().into()
    }

    /// Get the version of the Encore runtime.
    #[staticmethod]
    pub fn version() -> String {
        encore_runtime_core::version().to_string()
    }

    /// Get the git commit used to build the Encore runtime.
    #[staticmethod]
    pub fn build_commit() -> String {
        encore_runtime_core::build_commit().to_string()
    }

    /// Get the number of worker threads.
    pub fn num_worker_threads(&self) -> u32 {
        match self.runtime.compute().worker_threads {
            Some(n) => {
                if n > 0 {
                    n as u32
                } else {
                    num_cpus::get() as u32
                }
            }
            None => 1u32,
        }
    }
}
