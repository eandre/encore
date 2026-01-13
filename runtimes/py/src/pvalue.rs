use chrono::TimeZone;
use encore_runtime_core::api::{PValue, PValues};
use pyo3::prelude::*;
use pyo3::types::{PyBool, PyDict, PyFloat, PyInt, PyList, PyNone, PyString};
use serde_json::Number;

/// Convert a Python object to a PValue.
pub fn py_to_pvalue(obj: &Bound<'_, PyAny>) -> PyResult<PValue> {
    if obj.is_none() {
        return Ok(PValue::Null);
    }

    if let Ok(b) = obj.cast::<PyBool>() {
        return Ok(PValue::Bool(b.is_true()));
    }

    if let Ok(i) = obj.cast::<PyInt>() {
        let val: i64 = i.extract()?;
        return Ok(PValue::Number(Number::from(val)));
    }

    if let Ok(f) = obj.cast::<PyFloat>() {
        let val: f64 = f.extract()?;
        return Ok(PValue::Number(
            Number::from_f64(val).unwrap_or_else(|| Number::from(0)),
        ));
    }

    if let Ok(s) = obj.cast::<PyString>() {
        let val: String = s.extract()?;
        return Ok(PValue::String(val));
    }

    if let Ok(list) = obj.cast::<PyList>() {
        let mut arr = Vec::with_capacity(list.len());
        for item in list.iter() {
            arr.push(py_to_pvalue(&item)?);
        }
        return Ok(PValue::Array(arr));
    }

    if let Ok(dict) = obj.cast::<PyDict>() {
        let pvals = py_dict_to_pvalues(dict)?;
        return Ok(PValue::Object(pvals));
    }

    // Try to handle datetime by checking for timestamp method
    if obj.hasattr("timestamp")? {
        if let Ok(ts) = obj.call_method0("timestamp") {
            if let Ok(ts_val) = ts.extract::<f64>() {
                let secs = ts_val.trunc() as i64;
                let nanos = ((ts_val.fract()) * 1_000_000_000.0) as u32;
                if let Some(dt) = chrono::Utc.timestamp_opt(secs, nanos).single() {
                    return Ok(PValue::DateTime(dt.fixed_offset()));
                }
            }
        }
    }

    // Fallback: try to convert to string representation
    let repr: String = obj.str()?.extract()?;
    Ok(PValue::String(repr))
}

/// Convert a Python dict to PValues.
pub fn py_dict_to_pvalues(dict: &Bound<'_, PyDict>) -> PyResult<PValues> {
    let mut pvals = PValues::new();
    for (key, value) in dict.iter() {
        let key_str: String = key.extract()?;
        let pval = py_to_pvalue(&value)?;
        pvals.insert(key_str, pval);
    }
    Ok(pvals)
}

/// Convert a Python object to Option<PValues>.
pub fn py_to_pvalues(obj: &Bound<'_, PyAny>) -> PyResult<Option<PValues>> {
    if obj.is_none() {
        return Ok(None);
    }

    if let Ok(dict) = obj.cast::<PyDict>() {
        return Ok(Some(py_dict_to_pvalues(dict)?));
    }

    Err(pyo3::exceptions::PyTypeError::new_err(
        "Expected a dict or None",
    ))
}

/// Convert a PValue to a Python object.
pub fn pvalue_to_py(py: Python<'_>, val: &PValue) -> PyResult<Py<PyAny>> {
    match val {
        PValue::Null => Ok(PyNone::get(py).to_owned().into_any().unbind()),
        PValue::Bool(b) => Ok(b.into_pyobject(py)?.to_owned().into_any().unbind()),
        PValue::Number(n) => {
            if let Some(i) = n.as_i64() {
                Ok(i.into_pyobject(py)?.into_any().unbind())
            } else if let Some(u) = n.as_u64() {
                Ok(u.into_pyobject(py)?.into_any().unbind())
            } else if let Some(f) = n.as_f64() {
                Ok(f.into_pyobject(py)?.into_any().unbind())
            } else {
                Ok(PyNone::get(py).to_owned().into_any().unbind())
            }
        }
        PValue::Decimal(d) => {
            // Convert decimal to string representation for now
            // TODO: Use Python's decimal.Decimal type
            Ok(d.to_string().into_pyobject(py)?.into_any().unbind())
        }
        PValue::String(s) => Ok(s.into_pyobject(py)?.into_any().unbind()),
        PValue::Array(arr) => {
            let list = PyList::empty(py);
            for item in arr {
                list.append(pvalue_to_py(py, item)?)?;
            }
            Ok(list.into_any().unbind())
        }
        PValue::Object(obj) => {
            let dict = pvalues_to_py_dict(py, obj)?;
            Ok(dict.into_any().unbind())
        }
        PValue::Cookie(c) => {
            // Convert cookie to a dict representation
            let dict = PyDict::new(py);
            dict.set_item("name", &c.name)?;
            dict.set_item("value", pvalue_to_py(py, &c.value)?)?;
            Ok(dict.into_any().unbind())
        }
        PValue::DateTime(dt) => {
            let ts = dt.timestamp_millis() as f64 / 1000.0;
            let datetime_mod = py.import("datetime")?;
            let datetime_class = datetime_mod.getattr("datetime")?;
            let utc = datetime_mod.getattr("timezone")?.getattr("utc")?;
            let dt_obj = datetime_class.call_method1("fromtimestamp", (ts, utc))?;
            Ok(dt_obj.unbind())
        }
    }
}

/// Convert PValues to a Python dict.
pub fn pvalues_to_py_dict<'py>(py: Python<'py>, pvals: &PValues) -> PyResult<Bound<'py, PyDict>> {
    let dict = PyDict::new(py);
    for (key, val) in pvals.iter() {
        dict.set_item(key, pvalue_to_py(py, val)?)?;
    }
    Ok(dict)
}

/// Convert PValues to a Python object (dict or None).
pub fn pvalues_to_py(py: Python<'_>, pvals: Option<&PValues>) -> PyResult<Py<PyAny>> {
    match pvals {
        Some(pvals) => Ok(pvalues_to_py_dict(py, pvals)?.into_any().unbind()),
        None => Ok(PyNone::get(py).to_owned().into_any().unbind()),
    }
}
