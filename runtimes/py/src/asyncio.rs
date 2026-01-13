use pyo3::{Bound, PyAny, PyResult, Python, types::PyAnyMethods};

/// Check if an object is an asyncio Future.
/// Equivalent to asyncio.isfuture(awaitable).
#[allow(dead_code)]
pub fn isfuture<'p>(py: Python<'p>, awaitable: &Bound<'p, PyAny>) -> PyResult<bool> {
    // hasattr(obj.__class__, '_asyncio_future_blocking') and obj._asyncio_future_blocking is not None
    match awaitable.getattr_opt(pyo3::intern!(py, "_asyncio_future_blocking"))? {
        None => Ok(false),
        Some(val) => Ok(!val.is_none()),
    }
}

/// Check if an object is awaitable (has __await__ method).
/// This returns true for coroutines, asyncio futures, and any object with __await__.
pub fn isawaitable(obj: &Bound<'_, PyAny>) -> bool {
    obj.hasattr(pyo3::intern!(obj.py(), "__await__")).unwrap_or(false)
}
