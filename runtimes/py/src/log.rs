use crate::api::Request;
use crate::pvalue::py_to_pvalue;
use encore_runtime_core::error::{AppError, StackFrame, StackTrace};
use encore_runtime_core::log::Fields;
use encore_runtime_core::log::LogFromExternalRuntime;
use pyo3::prelude::*;
use pyo3::types::PyDict;

/// Log level enum.
#[pyclass(eq, eq_int)]
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum LogLevel {
    Trace = 1,
    Debug = 2,
    Info = 3,
    Warn = 4,
    Error = 5,
}

impl From<LogLevel> for log::LevelFilter {
    fn from(value: LogLevel) -> Self {
        match value {
            LogLevel::Trace => log::LevelFilter::Trace,
            LogLevel::Debug => log::LevelFilter::Debug,
            LogLevel::Info => log::LevelFilter::Info,
            LogLevel::Warn => log::LevelFilter::Warn,
            LogLevel::Error => log::LevelFilter::Error,
        }
    }
}

impl From<LogLevel> for log::Level {
    fn from(value: LogLevel) -> Self {
        match value {
            LogLevel::Trace => log::Level::Trace,
            LogLevel::Debug => log::Level::Debug,
            LogLevel::Info => log::Level::Info,
            LogLevel::Warn => log::Level::Warn,
            LogLevel::Error => log::Level::Error,
        }
    }
}

/// A logger for structured logging.
#[pyclass]
pub struct Logger {
    logger: encore_runtime_core::log::Logger,
}

impl Default for Logger {
    fn default() -> Self {
        Self::new()
    }
}

impl Logger {
    pub fn new() -> Self {
        Self {
            logger: encore_runtime_core::log::root().clone(),
        }
    }
}

#[pymethods]
impl Logger {
    /// Log a message.
    ///
    /// Args:
    ///     request: Optional request context for tracing.
    ///     level: The log level.
    ///     msg: The log message.
    ///     error: Optional exception to log.
    ///     caller: Optional caller information (file:line).
    ///     fields: Optional additional fields as a dict.
    #[pyo3(signature = (request, level, msg, error=None, caller=None, fields=None))]
    pub fn log(
        &self,
        py: Python<'_>,
        request: Option<&Request>,
        level: LogLevel,
        msg: String,
        error: Option<Bound<'_, pyo3::types::PyAny>>,
        caller: Option<String>,
        fields: Option<Bound<'_, PyDict>>,
    ) {
        let error = convert_error(py, error).unwrap_or_else(|err| {
            ::log::error!("couldn't convert error to app error: {err}");
            None
        });

        let fields = convert_fields(py, fields);

        let res = self.logger.log(
            request.map(|r| r.inner.as_ref()),
            level.into(),
            msg,
            error,
            caller,
            fields,
        );

        if let Err(err) = res {
            ::log::error!("logging failed: {err}");
        }
    }

    /// Create a new logger with the specified level.
    ///
    /// Args:
    ///     level: The minimum log level.
    ///
    /// Returns:
    ///     A new Logger instance with the specified level.
    pub fn with_level(&self, level: LogLevel) -> Self {
        Self {
            logger: self.logger.with_level(level.into()),
        }
    }

    /// Create a new logger with additional context fields.
    ///
    /// Args:
    ///     fields: Additional fields to include in all log messages.
    ///
    /// Returns:
    ///     A new Logger instance with the additional fields.
    pub fn with_fields(&self, py: Python<'_>, fields: Bound<'_, PyDict>) -> PyResult<Self> {
        let fields = convert_fields(py, Some(fields)).unwrap_or_default();

        Ok(Self {
            logger: self.logger.with(fields),
        })
    }

    fn __repr__(&self) -> String {
        "Logger()".to_string()
    }
}

fn convert_error(
    py: Python<'_>,
    input: Option<Bound<'_, pyo3::types::PyAny>>,
) -> PyResult<Option<AppError>> {
    let Some(input) = input else {
        return Ok(None);
    };

    // Try to get the message from the exception
    let message: String = if let Ok(msg) = input.getattr("args") {
        if let Ok(args) = msg.cast::<pyo3::types::PyTuple>() {
            if !args.is_empty() {
                args.get_item(0)?.str()?.to_string()
            } else {
                input.str()?.to_string()
            }
        } else {
            input.str()?.to_string()
        }
    } else {
        input.str()?.to_string()
    };

    // Try to get the stack trace
    let stack = parse_python_traceback(py, &input).unwrap_or_default();

    Ok(Some(AppError {
        message,
        stack,
        cause: None,
    }))
}

fn parse_python_traceback(
    py: Python<'_>,
    exc: &Bound<'_, pyo3::types::PyAny>,
) -> PyResult<StackTrace> {
    let mut frames = Vec::new();

    // Try to get __traceback__ attribute
    if let Ok(tb) = exc.getattr("__traceback__") {
        let traceback_mod = py.import("traceback")?;
        let extract_tb = traceback_mod.getattr("extract_tb")?;

        if let Ok(tb_list) = extract_tb.call1((tb,)) {
            for item in tb_list.try_iter()? {
                if let Ok(frame) = item {
                    let file: String = frame.getattr("filename")?.extract().unwrap_or_default();
                    let line: u32 = frame.getattr("lineno")?.extract().unwrap_or(0);
                    let function: Option<String> = frame.getattr("name")?.extract().ok();

                    // Skip frames from the standard library
                    if !file.contains("site-packages") && !file.starts_with("<") {
                        frames.push(StackFrame {
                            file,
                            line,
                            column: None,
                            module: None,
                            function,
                        });
                    }
                }
            }
        }
    }

    Ok(frames)
}

fn convert_fields(_py: Python<'_>, input: Option<Bound<'_, PyDict>>) -> Option<Fields> {
    let input = input?;
    if input.is_empty() {
        return None;
    }

    let mut fields = Fields::new();

    for (key, value) in input.iter() {
        let key_str: String = match key.extract() {
            Ok(k) => k,
            Err(_) => continue,
        };

        // Convert the Python value to a JSON value
        let json_val = match py_to_pvalue(&value) {
            Ok(pval) => pvalue_to_json(&pval),
            Err(_) => {
                // If conversion fails, use the string representation
                serde_json::Value::String(value.str().map(|s| s.to_string()).unwrap_or_default())
            }
        };

        fields.insert(key_str, json_val);
    }

    Some(fields)
}

fn pvalue_to_json(pval: &encore_runtime_core::api::PValue) -> serde_json::Value {
    use encore_runtime_core::api::PValue;

    match pval {
        PValue::Null => serde_json::Value::Null,
        PValue::Bool(b) => serde_json::Value::Bool(*b),
        PValue::Number(n) => serde_json::Value::Number(n.clone()),
        PValue::Decimal(d) => serde_json::Value::String(d.to_string()),
        PValue::String(s) => serde_json::Value::String(s.clone()),
        PValue::Array(arr) => serde_json::Value::Array(arr.iter().map(pvalue_to_json).collect()),
        PValue::Object(obj) => {
            let map: serde_json::Map<String, serde_json::Value> = obj
                .iter()
                .map(|(k, v)| (k.clone(), pvalue_to_json(v)))
                .collect();
            serde_json::Value::Object(map)
        }
        PValue::Cookie(c) => serde_json::Value::String(format!("{}={}", c.name, c.value)),
        PValue::DateTime(dt) => serde_json::Value::String(dt.to_rfc3339()),
    }
}
