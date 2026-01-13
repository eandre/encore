use std::fs;

use crate::app::AppDesc;

use super::codegen::CodegenParams;
use super::{
    App, BuildOutput, Builder, BuilderError, CmdSpec, CompileResult, DebugMode, Entrypoint,
};

#[derive(Debug)]
pub struct CompileParams<'py> {
    pub desc: &'py AppDesc<'py>,
    pub debug: DebugMode,
}

impl Builder<'_> {
    pub fn compile(&self, params: CompileParams) -> Result<CompileResult, BuilderError> {
        // Generate the entrypoint code
        self.generate_code(CodegenParams { desc: params.desc })?;

        let build_dir = self.app.root.join(".encore").join("build");
        fs::create_dir_all(&build_dir).map_err(BuilderError::GenerateCode)?;

        // For Python, we run the app using the Python interpreter
        // The entrypoint is the generated main module
        let entrypoint = Entrypoint {
            cmd: CmdSpec {
                command: vec![
                    "uv".to_string(),
                    "run".to_string(),
                    "-m".to_string(),
                    "encore_gen.internal.entrypoints.combined.main".to_string(),
                ],
                env: vec![],
                prioritized_files: vec![],
            },
            services: vec![], // TODO: populate from parse result
            gateways: vec![], // TODO: populate from parse result
            use_runtime_config_v2: true,
        };

        Ok(CompileResult {
            outputs: vec![BuildOutput {
                artifact_dir: build_dir,
                entrypoints: vec![entrypoint],
            }],
        })
    }
}
