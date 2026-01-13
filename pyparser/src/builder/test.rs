use std::path::Path;

use crate::app::AppDesc;

use super::{App, Builder, BuilderError, CmdSpec, TestResult};

#[derive(Debug)]
pub struct TestParams<'py> {
    pub parse: &'py AppDesc<'py>,
}

impl Builder<'_> {
    pub fn test(&self, _params: TestParams) -> Result<TestResult, BuilderError> {
        // Check if pytest is configured
        let pyproject_path = self.app.root.join("pyproject.toml");
        if !pyproject_path.exists() {
            log::info!("pyproject.toml not found, skipping tests");
            return Ok(TestResult { cmd: None });
        }

        // Return a pytest command using uv
        // Users can configure this in pyproject.toml
        Ok(TestResult {
            cmd: Some(CmdSpec {
                command: vec!["uv".to_string(), "run".to_string(), "pytest".to_string()],
                env: vec!["ENCORE_LOG_LEVEL=error".to_string()],
                prioritized_files: vec![],
            }),
        })
    }
}
