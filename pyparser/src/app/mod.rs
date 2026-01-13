//! Application descriptor for Python Encore applications.

use std::path::Path;

use crate::ast::loader::errors::{ParseError, ParseResult};
use crate::ast::loader::fileset::FileSet;
use crate::ast::schema::Parser;
use crate::encore::parser::meta::v1;
use crate::legacymeta::compute_meta;
use crate::parser::parser::ParsedApp;

/// Application descriptor containing parsed resources and metadata.
#[derive(Debug)]
pub struct AppDesc<'py> {
    /// The parse result from the parser.
    pub parse: ParsedApp<'py>,
    /// The legacy metadata.
    pub meta: v1::Data,
}

/// Validates the parse result and computes metadata.
///
/// Returns `Some(AppDesc)` if validation succeeds, `None` otherwise.
pub fn validate_and_describe<'py>(
    file_set: &FileSet,
    parse: ParsedApp<'py>,
    app_root: &Path,
    parser: &'py Parser<'py>,
) -> Result<AppDesc<'py>, anyhow::Error> {
    // TODO: Add validation logic similar to tsparser's AppValidator
    // For now, we just compute the metadata

    validate_parse(&parse)?;

    let meta = compute_meta(file_set, &parse, app_root, parser)?;

    Ok(AppDesc { parse, meta })
}

/// Validates the parse result.
fn validate_parse(parse: &ParsedApp<'_>) -> ParseResult<()> {
    // Validate API endpoint names are unique within each service
    validate_api_endpoints(parse)?;

    // Validate SQL database names are unique
    validate_sql_databases(parse)?;

    // Validate metric names are unique
    validate_metrics(parse)?;

    Ok(())
}

/// Validates that API endpoint names are unique within each service.
fn validate_api_endpoints(parse: &ParsedApp<'_>) -> ParseResult<()> {
    use crate::parser::resources::Resource;
    use std::collections::HashMap;

    let mut seen: HashMap<(String, String), crate::ast::loader::fileset::Span> = HashMap::new();

    for resource in &parse.resources {
        if let Resource::APIEndpoint(ep) = resource {
            let key = (ep.service_name.clone().unwrap_or_default(), ep.name.clone());
            if let Some(_prev) = seen.insert(key.clone(), ep.span) {
                return Err(ParseError::Parse {
                    span: ep.span,
                    message: format!(
                        "duplicate API endpoint '{}' in service '{}'",
                        key.1, key.0
                    ),
                });
            }
        }
    }

    Ok(())
}

/// Validates that SQL database names are unique.
fn validate_sql_databases(parse: &ParsedApp<'_>) -> ParseResult<()> {
    use crate::parser::resources::Resource;
    use std::collections::HashMap;

    let mut seen: HashMap<String, crate::ast::loader::fileset::Span> = HashMap::new();

    for resource in &parse.resources {
        if let Resource::SQLDatabase(db) = resource {
            if let Some(_prev) = seen.insert(db.name.clone(), db.span) {
                return Err(ParseError::Parse {
                    span: db.span,
                    message: format!("SQL database '{}' is defined multiple times", db.name),
                });
            }
        }
    }

    Ok(())
}

/// Validates that metric names are unique across the application.
fn validate_metrics(parse: &ParsedApp<'_>) -> ParseResult<()> {
    use crate::parser::resources::Resource;
    use std::collections::HashMap;

    let mut seen: HashMap<String, crate::ast::loader::fileset::Span> = HashMap::new();

    for resource in &parse.resources {
        if let Resource::Metric(m) = resource {
            if let Some(_prev) = seen.insert(m.name.clone(), m.span) {
                return Err(ParseError::Parse {
                    span: m.span,
                    message: format!(
                        "metric '{}' is defined multiple times; metrics must have unique names across the entire application",
                        m.name
                    ),
                });
            }
        }
    }

    Ok(())
}
