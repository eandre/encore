use std::{
    path::{Path, PathBuf},
    sync::Arc,
};

use handlebars::*;
use serde::{Deserialize, Serialize};
use thiserror::Error;

pub use codegen::CodegenParams;
pub use compile::CompileParams;
pub use parse::ParseParams;
pub use prepare::PrepareParams;
pub use test::TestParams;

use crate::ast::loader::{
    fileset::FileSet,
    module_resolver::{DefaultModuleResolver, ModuleResolver},
    parsectx, ModuleLoader,
};

mod codegen;
mod compile;
mod parse;
mod prepare;
mod test;

pub struct Builder<'builder> {
    pub app: App,
    pub wd: PathBuf,
    pub ctx: parsectx::Context,
    pub file_set: Arc<FileSet>,
    pub loader: ModuleLoader,
    pub reg: Handlebars<'builder>,
    pub entrypoint_combined_main: Template<'builder>,
}

impl Builder<'_> {
    pub fn new(app: App, wd: PathBuf) -> Result<Self, BuilderError> {
        let ctx = parsectx::Context {
            app_root: app.root.clone(),
        };
        let resolver: Arc<dyn ModuleResolver> =
            Arc::new(DefaultModuleResolver::new(vec![app.root.clone()]));
        let file_set = Arc::new(FileSet::new());
        let loader = ModuleLoader::new(resolver, file_set.clone());

        let mut reg = Handlebars::new();
        reg.register_helper("toJSON", Box::new(to_json));
        reg.register_helper("toPython", Box::new(to_python));
        reg.register_helper("snakeCase", Box::new(snake_case));

        let entrypoint_combined_main =
            Template::new(&mut reg, "combined_main", ENTRYPOINT_COMBINED_MAIN)
                .map_err(|e| BuilderError::Internal(e.into()))?;

        Ok(Self {
            app,
            wd,
            ctx,
            file_set,
            loader,
            reg,
            entrypoint_combined_main,
        })
    }
}

#[derive(Debug, Clone)]
pub struct App {
    pub root: PathBuf,
    pub platform_id: Option<String>,
    pub local_id: String,
}

impl App {
    /// Compute the relative path from the app root.
    fn rel_path<'b>(&self, path: &'b Path) -> Result<&'b Path, BuilderError> {
        let suffix = path.strip_prefix(&self.root).map_err(|err| {
            BuilderError::Internal(anyhow::anyhow!(
                "unable to compute relative path to app root from {path:?}: {err}"
            ))
        })?;
        Ok(suffix)
    }

    /// Compute the relative path from the app root as a String.
    fn rel_path_string(&self, path: &Path) -> Result<String, BuilderError> {
        let suffix = self.rel_path(path)?;
        let s = suffix
            .to_str()
            .ok_or(BuilderError::Internal(anyhow::anyhow!(
                "invalid path: {:?}",
                path
            )))?;
        Ok(s.to_string())
    }
}

struct Template<'a> {
    name: &'a str,
}

impl<'a> Template<'a> {
    fn new(reg: &mut Handlebars, name: &'a str, template_str: &str) -> Result<Self, BuilderError> {
        reg.register_template_string(name, template_str)
            .map_err(|e| BuilderError::Internal(e.into()))?;
        Ok(Self { name })
    }

    fn render(&self, reg: &Handlebars, data: &impl Serialize) -> Result<String, BuilderError> {
        reg.render(self.name, data)
            .map_err(|e| BuilderError::Internal(e.into()))
    }
}

const ENTRYPOINT_COMBINED_MAIN: &str =
    include_str!("templates/entrypoints/combined/main.py.handlebars");

fn to_json(
    h: &Helper<'_, '_>,
    _: &Handlebars<'_>,
    _: &Context,
    _rc: &mut RenderContext<'_, '_>,
    out: &mut dyn Output,
) -> HelperResult {
    let param = h
        .param(0)
        .map(|v| serde_json::to_string(v.value()).unwrap())
        .unwrap_or_default();
    out.write(param.as_ref())?;
    Ok(())
}

/// Converts a value to Python syntax.
/// - true/false -> True/False
/// - null -> None
/// - strings are quoted
/// - numbers are rendered as-is
fn to_python(
    h: &Helper<'_, '_>,
    _: &Handlebars<'_>,
    _: &Context,
    _rc: &mut RenderContext<'_, '_>,
    out: &mut dyn Output,
) -> HelperResult {
    let result = match h.param(0).map(|v| v.value()) {
        Some(serde_json::Value::Bool(true)) => "True".to_string(),
        Some(serde_json::Value::Bool(false)) => "False".to_string(),
        Some(serde_json::Value::Null) => "None".to_string(),
        Some(serde_json::Value::String(s)) => {
            format!("\"{}\"", s.replace('\\', "\\\\").replace('"', "\\\""))
        }
        Some(serde_json::Value::Number(n)) => n.to_string(),
        Some(serde_json::Value::Array(arr)) => {
            let items: Vec<String> = arr.iter().map(|v| value_to_python(v)).collect();
            format!("[{}]", items.join(", "))
        }
        Some(serde_json::Value::Object(obj)) => {
            let items: Vec<String> = obj
                .iter()
                .map(|(k, v)| format!("\"{}\": {}", k, value_to_python(v)))
                .collect();
            format!("{{{}}}", items.join(", "))
        }
        None => "None".to_string(),
    };
    out.write(&result)?;
    Ok(())
}

/// Helper to convert a JSON value to Python syntax.
fn value_to_python(value: &serde_json::Value) -> String {
    match value {
        serde_json::Value::Bool(true) => "True".to_string(),
        serde_json::Value::Bool(false) => "False".to_string(),
        serde_json::Value::Null => "None".to_string(),
        serde_json::Value::String(s) => {
            format!("\"{}\"", s.replace('\\', "\\\\").replace('"', "\\\""))
        }
        serde_json::Value::Number(n) => n.to_string(),
        serde_json::Value::Array(arr) => {
            let items: Vec<String> = arr.iter().map(value_to_python).collect();
            format!("[{}]", items.join(", "))
        }
        serde_json::Value::Object(obj) => {
            let items: Vec<String> = obj
                .iter()
                .map(|(k, v)| format!("\"{}\": {}", k, value_to_python(v)))
                .collect();
            format!("{{{}}}", items.join(", "))
        }
    }
}

fn snake_case(
    h: &Helper<'_, '_>,
    _: &Handlebars<'_>,
    _: &Context,
    _rc: &mut RenderContext<'_, '_>,
    out: &mut dyn Output,
) -> HelperResult {
    let param = h
        .param(0)
        .and_then(|v| v.value().as_str())
        .unwrap_or_default();
    // Convert to snake_case
    let snake = param
        .chars()
        .enumerate()
        .flat_map(|(i, c)| {
            if c.is_uppercase() && i > 0 {
                vec!['_', c.to_ascii_lowercase()]
            } else {
                vec![c.to_ascii_lowercase()]
            }
        })
        .collect::<String>();
    out.write(&snake)?;
    Ok(())
}

#[derive(Serialize, Debug)]
pub struct CompileResult {
    pub outputs: Vec<BuildOutput>,
}

#[derive(Serialize, Debug)]
pub struct BuildOutput {
    pub artifact_dir: PathBuf,
    pub entrypoints: Vec<Entrypoint>,
}

#[derive(Serialize, Debug)]
pub struct Entrypoint {
    pub cmd: CmdSpec,
    pub services: Vec<String>,
    pub gateways: Vec<String>,
    pub use_runtime_config_v2: bool,
}

pub type ArtifactString = String;

#[derive(Serialize, Debug)]
pub struct CmdSpec {
    pub command: Vec<ArtifactString>,
    pub env: Vec<ArtifactString>,
    pub prioritized_files: Vec<ArtifactString>,
}

#[derive(Serialize, Debug)]
pub struct TestResult {
    pub cmd: Option<CmdSpec>,
}

#[derive(Deserialize, Debug, Copy, Clone)]
#[serde(rename_all = "lowercase")]
pub enum DebugMode {
    Disabled,
    Enabled,
    Break,
}

#[derive(Debug, Error)]
pub enum BuilderError {
    #[error("pyproject.toml not found (expected at {0})")]
    PyprojectNotFound(PathBuf),
    #[error("failed to read pyproject.toml: {0}")]
    ReadPyproject(#[source] std::io::Error),
    #[error("failed to write pyproject.toml: {0}")]
    WritePyproject(#[source] std::io::Error),
    #[error("invalid pyproject.toml: {0}")]
    InvalidPyproject(String),
    #[error("unable to generate code: {0}")]
    GenerateCode(#[source] std::io::Error),
    #[error("internal error: {0}")]
    Internal(#[source] anyhow::Error),
}

/// An error that is rendered plainly, without a backtrace.
#[derive(Debug)]
pub struct PlainError(pub String);

impl std::fmt::Display for PlainError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}
