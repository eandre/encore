use crate::api::Request;
use crate::pvalue::{pvalue_to_py, py_to_pvalue};
use encore_runtime_core::sqldb;
use pyo3::prelude::*;
use pyo3::types::{PyBytes, PyDict, PyList};
use std::sync::{Arc, OnceLock};

/// A SQL database connection.
#[pyclass]
pub struct SQLDatabase {
    tokio_rt: tokio::runtime::Handle,
    db: Arc<dyn sqldb::Database>,
    pool: OnceLock<PyResult<sqldb::Pool>>,
    task_locals: pyo3_async_runtimes::TaskLocals,
}

impl SQLDatabase {
    pub fn new(
        tokio_rt: tokio::runtime::Handle,
        db: Arc<dyn sqldb::Database>,
        task_locals: pyo3_async_runtimes::TaskLocals,
    ) -> Self {
        Self {
            tokio_rt,
            db,
            pool: OnceLock::new(),
            task_locals,
        }
    }

    fn pool(&self) -> PyResult<&sqldb::Pool> {
        self.pool
            .get_or_init(|| {
                let _guard = self.tokio_rt.enter();
                self.db
                    .new_pool()
                    .map_err(|e| pyo3::exceptions::PyRuntimeError::new_err(e.to_string()))
            })
            .as_ref()
            .map_err(|e| pyo3::exceptions::PyRuntimeError::new_err(e.to_string()))
    }
}

#[pymethods]
impl SQLDatabase {
    /// Get the connection string for this database.
    ///
    /// Returns:
    ///     The connection string that can be used with standard database libraries.
    pub fn conn_string(&self) -> &str {
        self.db.proxy_conn_string()
    }

    /// Execute a query and return a cursor for iterating over results.
    ///
    /// Args:
    ///     query: The SQL query string.
    ///     args: Query arguments.
    ///     source: Optional source request for tracing.
    ///
    /// Returns:
    ///     A coroutine that resolves to a Cursor for iterating over query results.
    #[pyo3(signature = (query, args, source=None))]
    pub fn query<'py>(
        &self,
        py: Python<'py>,
        query: String,
        args: &QueryArgs,
        source: Option<&Request>,
    ) -> PyResult<Bound<'py, PyAny>> {
        let values: Vec<_> = args.values.lock().unwrap().drain(..).collect();
        let source = source.map(|s| s.inner.clone());
        let pool = self.pool()?.to_owned();
        let task_locals = self.task_locals.clone();

        pyo3_async_runtimes::tokio::future_into_py_with_locals(
            py,
            task_locals.clone(),
            async move {
                let stream = pool
                    .query_raw(&query, values, source.as_ref().map(|s| s.as_ref()))
                    .await
                    .map_err(|e| pyo3::exceptions::PyRuntimeError::new_err(e.to_string()))?;

                Ok(Cursor {
                    stream: tokio::sync::Mutex::new(stream),
                    task_locals,
                })
            },
        )
    }

    /// Execute a query and return a single row.
    ///
    /// Args:
    ///     query: The SQL query string.
    ///     args: Query arguments.
    ///     source: Optional source request for tracing.
    ///
    /// Returns:
    ///     A coroutine that resolves to a Row if found, or None.
    #[pyo3(signature = (query, args, source=None))]
    pub fn query_row<'py>(
        &self,
        py: Python<'py>,
        query: String,
        args: &QueryArgs,
        source: Option<&Request>,
    ) -> PyResult<Bound<'py, PyAny>> {
        let values: Vec<_> = args.values.lock().unwrap().drain(..).collect();
        let source = source.map(|s| s.inner.clone());
        let pool = self.pool()?.to_owned();
        let task_locals = self.task_locals.clone();

        pyo3_async_runtimes::tokio::future_into_py_with_locals(py, task_locals, async move {
            let stream_result =
                pool.query_raw(&query, values, source.as_ref().map(|s| s.as_ref()));
            match stream_result.await {
                Ok(mut stream) => {
                    let row = stream.next().await.transpose().map_err(|e| {
                        pyo3::exceptions::PyRuntimeError::new_err(e.to_string())
                    })?;
                    Ok(row.map(|row| Row { row }))
                }
                Err(e) => Err(pyo3::exceptions::PyRuntimeError::new_err(e.to_string())),
            }
        })
    }

    /// Begin a transaction.
    ///
    /// Args:
    ///     source: Optional source request for tracing.
    ///
    /// Returns:
    ///     A coroutine that resolves to a Transaction instance.
    #[pyo3(signature = (source=None))]
    pub fn begin<'py>(
        &self,
        py: Python<'py>,
        source: Option<&Request>,
    ) -> PyResult<Bound<'py, PyAny>> {
        let source = source.map(|s| s.inner.clone());
        let pool = self.pool()?.to_owned();
        let task_locals = self.task_locals.clone();

        pyo3_async_runtimes::tokio::future_into_py_with_locals(
            py,
            task_locals.clone(),
            async move {
                let tx = pool
                    .begin(source.as_ref().map(|s| s.as_ref()))
                    .await
                    .map_err(|e| pyo3::exceptions::PyRuntimeError::new_err(e.to_string()))?;

                Ok(Transaction {
                    tx: tokio::sync::Mutex::new(Some(tx)),
                    task_locals,
                })
            },
        )
    }

    fn __repr__(&self) -> String {
        "SQLDatabase()".to_string()
    }
}

/// Query arguments builder.
#[pyclass]
pub struct QueryArgs {
    pub(crate) values: std::sync::Mutex<Vec<sqldb::RowValue>>,
}

#[pymethods]
impl QueryArgs {
    /// Create new query arguments from a list of values.
    ///
    /// Args:
    ///     params: A list of parameter values.
    #[new]
    pub fn new(params: Bound<'_, PyList>) -> PyResult<Self> {
        let values = convert_row_values(&params)?;
        Ok(Self {
            values: std::sync::Mutex::new(values),
        })
    }

    fn __repr__(&self) -> String {
        let count = self.values.lock().unwrap().len();
        format!("QueryArgs({})", count)
    }
}

fn convert_row_values(params: &Bound<'_, PyList>) -> PyResult<Vec<sqldb::RowValue>> {
    let mut values = Vec::with_capacity(params.len());

    for item in params.iter() {
        // Check if it's bytes
        if let Ok(bytes) = item.cast::<PyBytes>() {
            values.push(sqldb::RowValue::Bytes(bytes.as_bytes().to_vec()));
            continue;
        }

        // Otherwise convert to PValue
        let pval = py_to_pvalue(&item)?;
        values.push(sqldb::RowValue::PVal(pval));
    }

    Ok(values)
}

/// A database transaction.
#[pyclass]
pub struct Transaction {
    tx: tokio::sync::Mutex<Option<sqldb::Transaction>>,
    task_locals: pyo3_async_runtimes::TaskLocals,
}

#[pymethods]
impl Transaction {
    /// Commit the transaction.
    ///
    /// Args:
    ///     source: Optional source request for tracing.
    ///
    /// Returns:
    ///     A coroutine that resolves when the commit is complete.
    #[pyo3(signature = (source=None))]
    pub fn commit<'py>(
        &self,
        py: Python<'py>,
        source: Option<&Request>,
    ) -> PyResult<Bound<'py, PyAny>> {
        let source = source.map(|s| s.inner.clone());
        let task_locals = self.task_locals.clone();

        // We need to get the transaction out of self before the async block
        // Since we can't move self into the async block, we use a channel
        let tx_mutex = unsafe {
            // SAFETY: We're cloning the Arc inside the Mutex wrapper
            std::ptr::read(&self.tx as *const tokio::sync::Mutex<Option<sqldb::Transaction>>)
        };

        pyo3_async_runtimes::tokio::future_into_py_with_locals(py, task_locals, async move {
            let tx = tx_mutex.lock().await.take().ok_or_else(|| {
                pyo3::exceptions::PyRuntimeError::new_err("transaction closed")
            })?;
            tx.commit(source.as_ref().map(|s| s.as_ref()))
                .await
                .map_err(|e| pyo3::exceptions::PyRuntimeError::new_err(e.to_string()))?;
            Ok(())
        })
    }

    /// Rollback the transaction.
    ///
    /// Args:
    ///     source: Optional source request for tracing.
    ///
    /// Returns:
    ///     A coroutine that resolves when the rollback is complete.
    #[pyo3(signature = (source=None))]
    pub fn rollback<'py>(
        &self,
        py: Python<'py>,
        source: Option<&Request>,
    ) -> PyResult<Bound<'py, PyAny>> {
        let source = source.map(|s| s.inner.clone());
        let task_locals = self.task_locals.clone();

        let tx_mutex = unsafe {
            std::ptr::read(&self.tx as *const tokio::sync::Mutex<Option<sqldb::Transaction>>)
        };

        pyo3_async_runtimes::tokio::future_into_py_with_locals(py, task_locals, async move {
            let tx = tx_mutex.lock().await.take().ok_or_else(|| {
                pyo3::exceptions::PyRuntimeError::new_err("transaction closed")
            })?;
            tx.rollback(source.as_ref().map(|s| s.as_ref()))
                .await
                .map_err(|e| pyo3::exceptions::PyRuntimeError::new_err(e.to_string()))?;
            Ok(())
        })
    }

    /// Execute a query within the transaction.
    ///
    /// Args:
    ///     query: The SQL query string.
    ///     args: Query arguments.
    ///     source: Optional source request for tracing.
    ///
    /// Returns:
    ///     A coroutine that resolves to a Cursor for iterating over query results.
    #[pyo3(signature = (query, args, source=None))]
    pub fn query<'py>(
        &self,
        py: Python<'py>,
        query: String,
        args: &QueryArgs,
        source: Option<&Request>,
    ) -> PyResult<Bound<'py, PyAny>> {
        let values: Vec<_> = args.values.lock().unwrap().drain(..).collect();
        let source = source.map(|s| s.inner.clone());
        let task_locals = self.task_locals.clone();

        let tx_mutex = unsafe {
            std::ptr::read(&self.tx as *const tokio::sync::Mutex<Option<sqldb::Transaction>>)
        };

        pyo3_async_runtimes::tokio::future_into_py_with_locals(
            py,
            task_locals.clone(),
            async move {
                let tx = tx_mutex.lock().await;
                let tx_ref = tx.as_ref().ok_or_else(|| {
                    pyo3::exceptions::PyRuntimeError::new_err("transaction closed")
                })?;
                let stream = tx_ref
                    .query_raw(&query, values, source.as_ref().map(|s| s.as_ref()))
                    .await
                    .map_err(|e| pyo3::exceptions::PyRuntimeError::new_err(e.to_string()))?;

                Ok(Cursor {
                    stream: tokio::sync::Mutex::new(stream),
                    task_locals,
                })
            },
        )
    }

    fn __repr__(&self) -> String {
        "Transaction()".to_string()
    }
}

/// A cursor for iterating over query results.
#[pyclass]
pub struct Cursor {
    stream: tokio::sync::Mutex<sqldb::Cursor>,
    task_locals: pyo3_async_runtimes::TaskLocals,
}

#[pymethods]
impl Cursor {
    /// Get the next row from the cursor.
    ///
    /// Returns:
    ///     A coroutine that resolves to the next Row, or None if exhausted.
    pub fn next<'py>(&self, py: Python<'py>) -> PyResult<Bound<'py, PyAny>> {
        let task_locals = self.task_locals.clone();

        let stream_mutex = unsafe {
            std::ptr::read(&self.stream as *const tokio::sync::Mutex<sqldb::Cursor>)
        };

        pyo3_async_runtimes::tokio::future_into_py_with_locals(py, task_locals, async move {
            let mut stream = stream_mutex.lock().await;
            let row = stream.next().await.transpose().map_err(|e| {
                pyo3::exceptions::PyRuntimeError::new_err(format!("{:#?}", e))
            })?;
            Ok(row.map(|row| Row { row }))
        })
    }

    fn __repr__(&self) -> String {
        "Cursor()".to_string()
    }
}

/// A row from a query result.
#[pyclass]
pub struct Row {
    row: sqldb::Row,
}

#[pymethods]
impl Row {
    /// Get all values from the row as a dict.
    ///
    /// Returns:
    ///     A dict mapping column names to values.
    pub fn values(&self, py: Python<'_>) -> PyResult<Py<PyAny>> {
        let vals = self
            .row
            .values()
            .map_err(|e| pyo3::exceptions::PyRuntimeError::new_err(e.to_string()))?;

        let dict = PyDict::new(py);
        for (key, val) in vals {
            let py_val = match val {
                sqldb::RowValue::PVal(pval) => pvalue_to_py(py, &pval)?,
                sqldb::RowValue::Bytes(bytes) => PyBytes::new(py, &bytes).into_any().unbind(),
                sqldb::RowValue::Uuid(uuid) => {
                    uuid.to_string().into_pyobject(py)?.into_any().unbind()
                }
                sqldb::RowValue::Cidr(cidr) => {
                    cidr.to_string().into_pyobject(py)?.into_any().unbind()
                }
                sqldb::RowValue::Inet(inet) => {
                    inet.to_string().into_pyobject(py)?.into_any().unbind()
                }
            };
            dict.set_item(key, py_val)?;
        }
        Ok(dict.into_any().unbind())
    }

    fn __repr__(&self) -> String {
        "Row()".to_string()
    }
}
