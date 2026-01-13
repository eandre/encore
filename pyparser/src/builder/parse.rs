//! Parse step for Python applications.

use crate::{
    app::{validate_and_describe, AppDesc},
    ast::schema::Parser,
    parser::parser,
};

use super::{Builder, BuilderError};

/// Parameters for parsing a Python application.
#[derive(Debug)]
pub struct ParseParams {
    /// Whether to parse test files.
    pub parse_tests: bool,
}

impl<'builder> Builder<'builder> {
    /// Parses the Python application and returns the application descriptor.
    pub fn parse<'py>(
        &'py self,
        parser: &'py Parser<'py>,
        _params: ParseParams,
    ) -> Result<AppDesc<'py>, BuilderError> {
        log::info!("parsing Python app at {:?}", self.app.root);

        // Create the parser and run it
        let result = parser::parse(&self.ctx, &self.loader, self.file_set.clone(), parser)
            .map_err(|e| {
                BuilderError::Internal(anyhow::anyhow!("failed to parse application: {}", e))
            })?;

        log::info!(
            "parsed {} resources, {} binds, {} services",
            result.resources.len(),
            result.binds.len(),
            result.services.len()
        );

        // Validate and compute metadata
        let desc =
            validate_and_describe(&self.file_set, result, &self.ctx.app_root, parser).map_err(|e| {
                BuilderError::Internal(anyhow::anyhow!("failed to validate application: {}", e))
            })?;

        log::info!(
            "computed metadata: {} services, {} endpoints, {} pubsub topics",
            desc.meta.svcs.len(),
            desc.meta.svcs.iter().map(|s| s.rpcs.len()).sum::<usize>(),
            desc.meta.pubsub_topics.len()
        );

        Ok(desc)
    }
}
