use crate::asyncio::isawaitable;
use crate::pvalue::{pvalues_to_py, py_to_pvalues};
use encore_runtime_core::api::{self, HandlerResponse, HandlerResponseInner, schema};
use encore_runtime_core::model::RequestData;
use pyo3::prelude::*;
use pyo3::types::PyDict;
use std::future::Future;
use std::pin::Pin;
use std::sync::Arc;

/// API route configuration.
#[pyclass]
pub struct APIRoute {
    #[pyo3(get, set)]
    pub service: String,
    #[pyo3(get, set)]
    pub name: String,
    #[pyo3(get, set)]
    pub raw: bool,
    #[pyo3(get, set)]
    pub streaming: bool,
    #[pyo3(get)]
    pub handler: Py<PyAny>,
}

#[pymethods]
impl APIRoute {
    #[new]
    #[pyo3(signature = (service, name, handler, raw=false, streaming=false))]
    pub fn new(
        service: String,
        name: String,
        handler: Py<PyAny>,
        raw: bool,
        streaming: bool,
    ) -> Self {
        Self {
            service,
            name,
            raw,
            streaming,
            handler,
        }
    }

    fn __repr__(&self) -> String {
        format!("APIRoute(service={:?}, name={:?})", self.service, self.name)
    }
}

/// An incoming API request.
#[pyclass]
pub struct Request {
    pub(crate) inner: Arc<encore_runtime_core::model::Request>,
}

impl Request {
    pub fn new(inner: Arc<encore_runtime_core::model::Request>) -> Self {
        Self { inner }
    }
}

#[pymethods]
impl Request {
    /// Get the request payload as a dict.
    pub fn payload(&self, py: Python<'_>) -> PyResult<Py<PyAny>> {
        match &self.inner.data {
            RequestData::RPC(data) => encode_request_payload(py, data.parsed_payload.as_ref()),
            RequestData::Auth(data) => encode_auth_payload(py, &data.parsed_payload),
            RequestData::PubSub(data) => pvalues_to_py(py, data.parsed_payload.as_ref()),
            RequestData::Stream(data) => encode_request_payload(py, data.parsed_payload.as_ref()),
        }
    }

    /// Get the authentication data, if any.
    pub fn get_auth_data(&self, py: Python<'_>) -> PyResult<Py<PyAny>> {
        use RequestData::*;
        match &self.inner.data {
            RPC(data) => pvalues_to_py(py, data.auth_data.as_ref()),
            Stream(data) => pvalues_to_py(py, data.auth_data.as_ref()),
            Auth(_) | PubSub(_) => Ok(py.None()),
        }
    }

    fn __repr__(&self) -> String {
        "Request()".to_string()
    }
}

fn encode_request_payload(py: Python<'_>, p: Option<&api::RequestPayload>) -> PyResult<Py<PyAny>> {
    let Some(p) = p else {
        return Ok(py.None());
    };

    let dict = PyDict::new(py);

    // Add fields from different parts of the request
    add_fields_to_dict(py, &dict, p.path.as_ref())?;
    add_fields_to_dict(py, &dict, p.query.as_ref())?;
    add_fields_to_dict(py, &dict, p.header.as_ref())?;
    add_fields_to_dict(py, &dict, p.cookie.as_ref())?;

    match &p.body {
        api::Body::Typed(typed) => add_fields_to_dict(py, &dict, typed.as_ref())?,
        api::Body::Raw(_) => {}
    }

    Ok(dict.into_any().unbind())
}

fn encode_auth_payload(py: Python<'_>, p: &api::auth::AuthPayload) -> PyResult<Py<PyAny>> {
    let dict = PyDict::new(py);
    add_fields_to_dict(py, &dict, p.query.as_ref())?;
    add_fields_to_dict(py, &dict, p.header.as_ref())?;
    add_fields_to_dict(py, &dict, p.cookie.as_ref())?;
    Ok(dict.into_any().unbind())
}

fn add_fields_to_dict<'a, I: IntoIterator<Item = (&'a String, &'a api::PValue)>>(
    py: Python<'_>,
    dict: &Bound<'_, PyDict>,
    vals: Option<I>,
) -> PyResult<()> {
    let Some(vals) = vals else {
        return Ok(());
    };

    for (k, v) in vals.into_iter() {
        let py_val = crate::pvalue::pvalue_to_py(py, v)?;
        dict.set_item(k, py_val)?;
    }
    Ok(())
}

/// Create a new API handler from a Python callable.
pub fn new_api_handler(
    py: Python<'_>,
    func: &Py<PyAny>,
    raw: bool,
    streaming: bool,
    resp_schema: Option<Arc<schema::Response>>,
    task_locals: pyo3_async_runtimes::TaskLocals,
) -> PyResult<Arc<dyn api::BoxedHandler>> {
    if streaming {
        return Err(pyo3::exceptions::PyNotImplementedError::new_err(
            "streaming handlers are not yet supported",
        ));
    }

    if raw {
        return Err(pyo3::exceptions::PyNotImplementedError::new_err(
            "raw handlers are not yet supported",
        ));
    }

    Ok(Arc::new(PyTypedHandler {
        handler: func.clone_ref(py),
        resp_schema,
        task_locals: task_locals,
    }))
}

/// A Python typed API handler.
struct PyTypedHandler {
    handler: Py<PyAny>,
    resp_schema: Option<Arc<schema::Response>>,
    task_locals: pyo3_async_runtimes::TaskLocals,
}

/// Result of calling the Python handler - either a direct value or a future to await.
enum CallResult {
    /// A synchronous result that can be parsed directly.
    Sync(PyResult<HandlerResponse>),
    /// An async result that needs to be awaited.
    Async(Pin<Box<dyn Future<Output = PyResult<Py<PyAny>>> + Send>>),
}

impl api::BoxedHandler for PyTypedHandler {
    fn call(
        self: Arc<Self>,
        req: api::HandlerRequest,
    ) -> Pin<Box<dyn Future<Output = api::ResponseData> + Send + 'static>> {
        Box::pin(async move {
            // First, call the Python handler and check if it returns an awaitable
            let call_result: PyResult<CallResult> = Python::attach(|py| {
                // Create the Request object
                let request = Request::new(req);
                let py_request = Py::new(py, request)?;

                // Call the handler
                let result = self.handler.call1(py, (py_request,));

                match result {
                    Ok(ret) => {
                        log::info!("got ok result");
                        let ret_bound = ret.bind(py);

                        // Check if the result is awaitable (coroutine, future, etc.)
                        if isawaitable(ret_bound) {
                            log::info!("result is awaitable");
                            // Convert the awaitable to a Rust future using the task locals
                            let future = pyo3_async_runtimes::into_future_with_locals(
                                &self.task_locals,
                                ret_bound.clone(),
                            )?;
                            log::info!("converted to future");
                            Ok(CallResult::Async(Box::pin(future)))
                        } else {
                            // Synchronous result - parse it directly
                            log::info!("result is not awaitable");
                            let response = parse_handler_response(py, ret_bound, &self.resp_schema);
                            log::info!("parsed response");
                            Ok(CallResult::Sync(response))
                        }
                    }
                    Err(e) => {
                        log::info!("got error result");
                        // Convert Python exception to API error
                        Ok(CallResult::Sync(Ok(Err(api::Error {
                            code: api::ErrCode::Internal,
                            message: api::ErrCode::Internal.default_public_message().into(),
                            internal_message: Some(e.to_string()),
                            stack: None,
                            details: None,
                        }))))
                    }
                }
            });

            // Process the result
            let resp = match call_result {
                Ok(CallResult::Sync(result)) => match result {
                    Ok(Ok(resp)) => Ok(resp),
                    Ok(Err(err)) => Err(err),
                    Err(e) => Err(api::Error {
                        code: api::ErrCode::Internal,
                        message: api::ErrCode::Internal.default_public_message().into(),
                        internal_message: Some(e.to_string()),
                        stack: None,
                        details: None,
                    }),
                },
                Ok(CallResult::Async(future)) => {
                    // Await the Python coroutine
                    match future.await {
                        Ok(ret) => {
                            // Parse the async result
                            Python::attach(|py| {
                                match parse_handler_response(py, ret.bind(py), &self.resp_schema) {
                                    Ok(Ok(resp)) => Ok(resp),
                                    Ok(Err(err)) => Err(err),
                                    Err(e) => Err(api::Error {
                                        code: api::ErrCode::Internal,
                                        message: api::ErrCode::Internal
                                            .default_public_message()
                                            .into(),
                                        internal_message: Some(e.to_string()),
                                        stack: None,
                                        details: None,
                                    }),
                                }
                            })
                        }
                        Err(e) => {
                            // Python exception from async handler
                            Err(api::Error {
                                code: api::ErrCode::Internal,
                                message: api::ErrCode::Internal.default_public_message().into(),
                                internal_message: Some(e.to_string()),
                                stack: None,
                                details: None,
                            })
                        }
                    }
                }
                Err(e) => Err(api::Error {
                    code: api::ErrCode::Internal,
                    message: api::ErrCode::Internal.default_public_message().into(),
                    internal_message: Some(e.to_string()),
                    stack: None,
                    details: None,
                }),
            };

            api::ResponseData::Typed(resp)
        })
    }
}

// Ensure PyTypedHandler is Send + Sync
unsafe impl Send for PyTypedHandler {}
unsafe impl Sync for PyTypedHandler {}

fn parse_handler_response(
    _py: Python<'_>,
    ret: &Bound<'_, PyAny>,
    _resp_schema: &Option<Arc<schema::Response>>,
) -> PyResult<HandlerResponse> {
    // If the return value is None, return empty response
    if ret.is_none() {
        return Ok(Ok(HandlerResponseInner {
            payload: None,
            extra_headers: None,
            status: None,
        }));
    }

    // Try to get payload from dict
    let payload = if let Ok(dict) = ret.cast::<PyDict>() {
        // Check if it's a response object with 'payload' key
        if let Ok(Some(payload_val)) = dict.get_item("payload") {
            py_to_pvalues(&payload_val)?
        } else {
            // Treat the whole dict as the payload
            Some(crate::pvalue::py_dict_to_pvalues(dict)?)
        }
    } else {
        // Try to convert directly
        py_to_pvalues(ret)?
    };

    Ok(Ok(HandlerResponseInner {
        payload,
        extra_headers: None,
        status: None,
    }))
}
