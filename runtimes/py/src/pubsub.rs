use crate::api::Request;
use crate::asyncio::isawaitable;
use crate::pvalue::py_dict_to_pvalues;
use encore_runtime_core::pubsub::{SubscriptionObj, TopicObj};
use encore_runtime_core::{api, model, pubsub};
use pyo3::prelude::*;
use pyo3::types::PyDict;
use std::future::Future;
use std::pin::Pin;
use std::sync::Arc;

/// A PubSub topic for publishing messages.
#[pyclass]
pub struct PubSubTopic {
    topic: Arc<TopicObj>,
    task_locals: pyo3_async_runtimes::TaskLocals,
}

impl PubSubTopic {
    pub fn new(topic: TopicObj, task_locals: pyo3_async_runtimes::TaskLocals) -> Self {
        Self {
            topic: Arc::new(topic),
            task_locals,
        }
    }
}

#[pymethods]
impl PubSubTopic {
    /// Publish a message to the topic.
    ///
    /// Args:
    ///     body: The message body as a dict.
    ///     source: Optional source request for tracing.
    ///
    /// Returns:
    ///     The message ID.
    #[pyo3(signature = (body, source=None))]
    pub fn publish<'py>(
        &self,
        py: Python<'py>,
        body: Bound<'_, PyDict>,
        source: Option<&Request>,
    ) -> PyResult<Bound<'py, PyAny>> {
        let payload = py_dict_to_pvalues(&body)?;
        let source = source.map(|s| s.inner.clone());
        let topic = self.topic.clone();
        let task_locals = self.task_locals.clone();

        pyo3_async_runtimes::tokio::future_into_py_with_locals(py, task_locals, async move {
            topic
                .publish(payload, source)
                .await
                .map_err(|e| pyo3::exceptions::PyRuntimeError::new_err(format!("{}", e)))
        })
    }

    fn __repr__(&self) -> String {
        "PubSubTopic()".to_string()
    }
}

/// Configuration for a PubSub subscription.
#[pyclass]
pub struct PubSubSubscriptionConfig {
    #[pyo3(get, set)]
    pub topic_name: String,
    #[pyo3(get, set)]
    pub subscription_name: String,
    #[pyo3(get)]
    pub handler: Py<PyAny>,
}

#[pymethods]
impl PubSubSubscriptionConfig {
    #[new]
    pub fn new(topic_name: String, subscription_name: String, handler: Py<PyAny>) -> Self {
        Self {
            topic_name,
            subscription_name,
            handler,
        }
    }

    fn __repr__(&self) -> String {
        format!(
            "PubSubSubscriptionConfig(topic={:?}, subscription={:?})",
            self.topic_name, self.subscription_name
        )
    }
}

/// A PubSub subscription handle returned to Python.
#[pyclass]
pub struct PubSubSubscription {}

impl PubSubSubscription {
    pub fn new() -> Self {
        Self {}
    }
}

#[pymethods]
impl PubSubSubscription {
    fn __repr__(&self) -> String {
        "PubSubSubscription()".to_string()
    }
}

/// Start processing messages for a subscription. Should be called from a tokio task.
pub async fn subscribe(
    sub: Arc<SubscriptionObj>,
    handler: Py<PyAny>,
    task_locals: pyo3_async_runtimes::TaskLocals,
) -> Result<(), api::Error> {
    let py_handler = Python::attach(|py| PySubscriptionHandler {
        handler: handler.clone_ref(py),
        task_locals,
    });

    sub.subscribe(Arc::new(py_handler)).await
}

/// Result of calling the Python handler - either a direct value or a future to await.
enum CallResult {
    /// A synchronous result that completed.
    Sync(Result<(), api::Error>),
    /// An async result that needs to be awaited.
    Async(Pin<Box<dyn Future<Output = PyResult<Py<PyAny>>> + Send>>),
}

/// Python subscription handler that bridges to the user's handler.
struct PySubscriptionHandler {
    handler: Py<PyAny>,
    task_locals: pyo3_async_runtimes::TaskLocals,
}

impl std::fmt::Debug for PySubscriptionHandler {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("PySubscriptionHandler").finish()
    }
}

impl pubsub::SubscriptionHandler for PySubscriptionHandler {
    fn handle_message(
        &self,
        msg: Arc<model::Request>,
    ) -> Pin<Box<dyn Future<Output = Result<(), api::Error>> + Send + '_>> {
        Box::pin(async move {
            // First, call the Python handler and check if it returns an awaitable
            let call_result: Result<CallResult, api::Error> = Python::attach(|py| {
                let request = Request::new(msg);
                let py_request = Py::new(py, request).map_err(|e| api::Error {
                    code: api::ErrCode::Internal,
                    message: api::ErrCode::Internal.default_public_message().into(),
                    internal_message: Some(e.to_string()),
                    stack: None,
                    details: None,
                })?;

                // Call the handler
                let result = self.handler.call1(py, (py_request,));

                match result {
                    Ok(ret) => {
                        let ret_bound = ret.bind(py);

                        // Check if the result is awaitable (coroutine, future, etc.)
                        if isawaitable(ret_bound) {
                            // Convert the awaitable to a Rust future using the task locals
                            let future = pyo3_async_runtimes::into_future_with_locals(
                                &self.task_locals,
                                ret_bound.clone(),
                            )
                            .map_err(|e| api::Error {
                                code: api::ErrCode::Internal,
                                message: api::ErrCode::Internal.default_public_message().into(),
                                internal_message: Some(e.to_string()),
                                stack: None,
                                details: None,
                            })?;
                            Ok(CallResult::Async(Box::pin(future)))
                        } else {
                            // Synchronous result - handler completed successfully
                            Ok(CallResult::Sync(Ok(())))
                        }
                    }
                    Err(e) => {
                        // Python exception from handler call
                        Ok(CallResult::Sync(Err(api::Error {
                            code: api::ErrCode::Internal,
                            message: api::ErrCode::Internal.default_public_message().into(),
                            internal_message: Some(e.to_string()),
                            stack: None,
                            details: None,
                        })))
                    }
                }
            });

            // Process the result
            match call_result {
                Ok(CallResult::Sync(result)) => result,
                Ok(CallResult::Async(future)) => {
                    // Await the Python coroutine
                    match future.await {
                        Ok(_) => Ok(()),
                        Err(e) => Err(api::Error {
                            code: api::ErrCode::Internal,
                            message: api::ErrCode::Internal.default_public_message().into(),
                            internal_message: Some(e.to_string()),
                            stack: None,
                            details: None,
                        }),
                    }
                }
                Err(e) => Err(e),
            }
        })
    }
}

// Ensure PySubscriptionHandler is Send + Sync
unsafe impl Send for PySubscriptionHandler {}
unsafe impl Sync for PySubscriptionHandler {}
