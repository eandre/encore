use crate::api::Request;
use encore_runtime_core::objects as core;
use pyo3::prelude::*;
use pyo3::types::PyBytes;
use std::sync::Arc;
use std::time::Duration;

/// An object storage bucket.
#[pyclass]
pub struct Bucket {
    bkt: Arc<core::Bucket>,
}

impl Bucket {
    pub fn new(bkt: core::Bucket) -> Self {
        Self { bkt: Arc::new(bkt) }
    }
}

#[pymethods]
impl Bucket {
    /// Get an object reference by name.
    ///
    /// Args:
    ///     name: The object name/key.
    ///
    /// Returns:
    ///     A BucketObject for operations on that object.
    pub fn object(&self, name: String) -> BucketObject {
        BucketObject::new(self.bkt.object(name))
    }

    /// List objects in the bucket.
    ///
    /// Args:
    ///     prefix: Optional prefix to filter objects.
    ///     limit: Optional maximum number of objects to return.
    ///     source: Optional source request for tracing.
    ///
    /// Returns:
    ///     A ListIterator for iterating over objects.
    #[pyo3(signature = (prefix=None, limit=None, source=None))]
    pub fn list(
        &self,
        py: Python<'_>,
        prefix: Option<String>,
        limit: Option<i64>,
        source: Option<&Request>,
    ) -> PyResult<ListIterator> {
        let options = core::ListOptions {
            prefix,
            limit: limit.map(|v| v as u64),
        };
        let source = source.map(|s| s.inner.clone());
        let bkt = self.bkt.clone();

        let iter = py.detach(move || {
            tokio::runtime::Handle::current().block_on(async move { bkt.list(options, source).await })
        });

        match iter {
            Ok(iter) => Ok(ListIterator::new(iter)),
            Err(e) => Err(pyo3::exceptions::PyRuntimeError::new_err(e.to_string())),
        }
    }

    fn __repr__(&self) -> String {
        "Bucket()".to_string()
    }
}

/// A reference to an object in a bucket.
#[pyclass]
pub struct BucketObject {
    obj: Arc<core::Object>,
}

impl BucketObject {
    pub fn new(obj: core::Object) -> Self {
        Self { obj: Arc::new(obj) }
    }
}

#[pymethods]
impl BucketObject {
    /// Get the object's attributes.
    ///
    /// Args:
    ///     version: Optional version to get attributes for.
    ///     source: Optional source request for tracing.
    ///
    /// Returns:
    ///     ObjectAttrs containing the object's metadata.
    #[pyo3(signature = (version=None, source=None))]
    pub fn attrs(
        &self,
        py: Python<'_>,
        version: Option<String>,
        source: Option<&Request>,
    ) -> PyResult<ObjectAttrs> {
        let options = core::AttrsOptions { version };
        let source = source.map(|s| s.inner.clone());
        let obj = self.obj.clone();

        let attrs = py.detach(move || {
            tokio::runtime::Handle::current()
                .block_on(async move { obj.attrs(options, source).await })
        });

        match attrs {
            Ok(attrs) => Ok(attrs.into()),
            Err(e) => Err(object_error_to_py_err(e)),
        }
    }

    /// Check if the object exists.
    ///
    /// Args:
    ///     version: Optional version to check.
    ///     source: Optional source request for tracing.
    ///
    /// Returns:
    ///     True if the object exists, False otherwise.
    #[pyo3(signature = (version=None, source=None))]
    pub fn exists(
        &self,
        py: Python<'_>,
        version: Option<String>,
        source: Option<&Request>,
    ) -> PyResult<bool> {
        let options = core::ExistsOptions { version };
        let source = source.map(|s| s.inner.clone());
        let obj = self.obj.clone();

        let exists = py.detach(move || {
            tokio::runtime::Handle::current()
                .block_on(async move { obj.exists(options, source).await })
        });

        match exists {
            Ok(v) => Ok(v),
            Err(e) => Err(object_error_to_py_err(e)),
        }
    }

    /// Upload data to the object.
    ///
    /// Args:
    ///     data: The data to upload as bytes.
    ///     content_type: Optional content type.
    ///     not_exists: If True, fail if the object already exists.
    ///     source: Optional source request for tracing.
    ///
    /// Returns:
    ///     ObjectAttrs of the uploaded object.
    #[pyo3(signature = (data, content_type=None, not_exists=None, source=None))]
    pub fn upload(
        &self,
        py: Python<'_>,
        data: Bound<'_, PyBytes>,
        content_type: Option<String>,
        not_exists: Option<bool>,
        source: Option<&Request>,
    ) -> PyResult<ObjectAttrs> {
        let data_vec = data.as_bytes().to_vec();
        let cursor = std::io::Cursor::new(data_vec);

        let options = core::UploadOptions {
            content_type,
            preconditions: not_exists.map(|ne| core::UploadPreconditions {
                not_exists: Some(ne),
            }),
        };
        let source = source.map(|s| s.inner.clone());
        let obj = self.obj.clone();

        let attrs = py.detach(move || {
            tokio::runtime::Handle::current()
                .block_on(async move { obj.upload(Box::new(cursor), options, source).await })
        });

        match attrs {
            Ok(attrs) => Ok(attrs.into()),
            Err(e) => Err(object_error_to_py_err(e)),
        }
    }

    /// Download all data from the object.
    ///
    /// Args:
    ///     version: Optional version to download.
    ///     source: Optional source request for tracing.
    ///
    /// Returns:
    ///     The object data as bytes.
    #[pyo3(signature = (version=None, source=None))]
    pub fn download_all(
        &self,
        py: Python<'_>,
        version: Option<String>,
        source: Option<&Request>,
    ) -> PyResult<Py<PyAny>> {
        let options = core::DownloadOptions { version };
        let source = source.map(|s| s.inner.clone());
        let obj = self.obj.clone();

        let data = py.detach(move || {
            tokio::runtime::Handle::current()
                .block_on(async move { obj.download_all(options, source).await.map(Vec::from) })
        });

        match data {
            Ok(data) => Ok(PyBytes::new(py, &data).into_any().unbind()),
            Err(e) => Err(object_error_to_py_err(e)),
        }
    }

    /// Delete the object.
    ///
    /// Args:
    ///     version: Optional version to delete.
    ///     source: Optional source request for tracing.
    #[pyo3(signature = (version=None, source=None))]
    pub fn delete(
        &self,
        py: Python<'_>,
        version: Option<String>,
        source: Option<&Request>,
    ) -> PyResult<()> {
        let options = core::DeleteOptions { version };
        let source = source.map(|s| s.inner.clone());
        let obj = self.obj.clone();

        let result = py.detach(move || {
            tokio::runtime::Handle::current()
                .block_on(async move { obj.delete(options, source).await })
        });

        match result {
            Ok(()) => Ok(()),
            Err(e) => Err(object_error_to_py_err(e)),
        }
    }

    /// Get a signed upload URL.
    ///
    /// Args:
    ///     ttl: Time-to-live in seconds (default: 3600).
    ///     source: Optional source request for tracing.
    ///
    /// Returns:
    ///     The signed upload URL.
    #[pyo3(signature = (ttl=None, source=None))]
    pub fn signed_upload_url(
        &self,
        py: Python<'_>,
        ttl: Option<i64>,
        source: Option<&Request>,
    ) -> PyResult<String> {
        let options = core::UploadUrlOptions {
            ttl: Duration::from_secs(ttl.map(|v| v as u64).unwrap_or(3600)),
        };
        let source = source.map(|s| s.inner.clone());
        let obj = self.obj.clone();

        let url = py.detach(move || {
            tokio::runtime::Handle::current()
                .block_on(async move { obj.signed_upload_url(options, source).await })
        });

        match url {
            Ok(url) => Ok(url),
            Err(e) => Err(object_error_to_py_err(e)),
        }
    }

    /// Get a signed download URL.
    ///
    /// Args:
    ///     ttl: Time-to-live in seconds (default: 3600).
    ///     source: Optional source request for tracing.
    ///
    /// Returns:
    ///     The signed download URL.
    #[pyo3(signature = (ttl=None, source=None))]
    pub fn signed_download_url(
        &self,
        py: Python<'_>,
        ttl: Option<i64>,
        source: Option<&Request>,
    ) -> PyResult<String> {
        let options = core::DownloadUrlOptions {
            ttl: Duration::from_secs(ttl.map(|v| v as u64).unwrap_or(3600)),
        };
        let source = source.map(|s| s.inner.clone());
        let obj = self.obj.clone();

        let url = py.detach(move || {
            tokio::runtime::Handle::current()
                .block_on(async move { obj.signed_download_url(options, source).await })
        });

        match url {
            Ok(url) => Ok(url),
            Err(e) => Err(object_error_to_py_err(e)),
        }
    }

    /// Get the public URL for the object.
    ///
    /// Returns:
    ///     The public URL, if the bucket is configured for public access.
    pub fn public_url(&self) -> PyResult<String> {
        self.obj
            .public_url()
            .map_err(|e| pyo3::exceptions::PyRuntimeError::new_err(e.to_string()))
    }

    fn __repr__(&self) -> String {
        "BucketObject()".to_string()
    }
}

/// Object attributes/metadata.
#[pyclass]
#[derive(Clone)]
pub struct ObjectAttrs {
    #[pyo3(get)]
    pub name: String,
    #[pyo3(get)]
    pub version: Option<String>,
    #[pyo3(get)]
    pub size: i64,
    #[pyo3(get)]
    pub content_type: Option<String>,
    #[pyo3(get)]
    pub etag: String,
}

impl From<core::ObjectAttrs> for ObjectAttrs {
    fn from(value: core::ObjectAttrs) -> Self {
        Self {
            name: value.name,
            version: value.version,
            size: value.size as i64,
            content_type: value.content_type,
            etag: value.etag,
        }
    }
}

#[pymethods]
impl ObjectAttrs {
    fn __repr__(&self) -> String {
        format!("ObjectAttrs(name={:?}, size={})", self.name, self.size)
    }
}

/// An entry from listing objects.
#[pyclass]
#[derive(Clone)]
pub struct ListEntry {
    #[pyo3(get)]
    pub name: String,
    #[pyo3(get)]
    pub size: i64,
    #[pyo3(get)]
    pub etag: String,
}

impl From<core::ListEntry> for ListEntry {
    fn from(value: core::ListEntry) -> Self {
        Self {
            name: value.name,
            size: value.size as i64,
            etag: value.etag,
        }
    }
}

#[pymethods]
impl ListEntry {
    fn __repr__(&self) -> String {
        format!("ListEntry(name={:?}, size={})", self.name, self.size)
    }
}

/// Iterator for listing objects in a bucket.
#[pyclass]
pub struct ListIterator {
    stream: tokio::sync::Mutex<Option<core::ListIterator>>,
}

impl ListIterator {
    fn new(stream: core::ListIterator) -> Self {
        Self {
            stream: tokio::sync::Mutex::new(Some(stream)),
        }
    }
}

#[pymethods]
impl ListIterator {
    /// Get the next entry from the iterator.
    ///
    /// Returns:
    ///     The next ListEntry, or None if exhausted.
    pub fn next(&self, py: Python<'_>) -> PyResult<Option<ListEntry>> {
        py.detach(|| {
            tokio::runtime::Handle::current().block_on(async {
                let mut stream = self.stream.lock().await;
                if let Some(stream) = stream.as_mut() {
                    let entry = stream
                        .next()
                        .await
                        .transpose()
                        .map_err(|e| pyo3::exceptions::PyRuntimeError::new_err(format!("{:#?}", e)))?;
                    Ok(entry.map(ListEntry::from))
                } else {
                    Err(pyo3::exceptions::PyRuntimeError::new_err(
                        "iterator is closed",
                    ))
                }
            })
        })
    }

    /// Mark the iterator as done (releases resources).
    pub fn mark_done(&self, py: Python<'_>) {
        py.detach(|| {
            tokio::runtime::Handle::current().block_on(async {
                let mut stream = self.stream.lock().await;
                if let Some(s) = stream.take() {
                    drop(s);
                }
            })
        });
    }

    fn __iter__(slf: PyRef<'_, Self>) -> PyRef<'_, Self> {
        slf
    }

    fn __next__(&self, py: Python<'_>) -> PyResult<Option<ListEntry>> {
        self.next(py)
    }

    fn __repr__(&self) -> String {
        "ListIterator()".to_string()
    }
}

fn object_error_to_py_err(err: core::Error) -> PyErr {
    match err {
        core::Error::NotFound => pyo3::exceptions::PyFileNotFoundError::new_err("object not found"),
        core::Error::PreconditionFailed => {
            pyo3::exceptions::PyRuntimeError::new_err("precondition failed")
        }
        core::Error::InvalidArgument => {
            pyo3::exceptions::PyValueError::new_err("invalid argument")
        }
        core::Error::Internal(e) => pyo3::exceptions::PyRuntimeError::new_err(e.to_string()),
        core::Error::Other(e) => pyo3::exceptions::PyRuntimeError::new_err(e.to_string()),
    }
}
