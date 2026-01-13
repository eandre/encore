//! Cron job resource definition and parsing.

use std::sync::Arc;

use pylitparser::LitParser;
use pylitparser_derive::LitParser;
use ruff_text_size::{Ranged, TextRange};

use crate::ast::loader::errors::ParseResult;
use crate::ast::loader::fileset::Span;
use crate::ast::scoping::references::{iter_references, ReferenceContext, ReferenceParser, TrackedNames};
use crate::parser::resourceparser::{ResourceParseContext, ResourceParser};
use crate::parser::resources::{BindKind, Resource};

/// A cron job definition.
#[derive(Debug, Clone)]
pub struct CronJob {
    /// The cron job name.
    pub name: String,
    /// Source range of the cron job definition.
    pub span: Span,
    /// Title for the cron job.
    pub title: Option<String>,
    /// Duration string for how often to run (e.g., "1h", "30m").
    pub every: Option<String>,
    /// Cron expression schedule (e.g., "0 0 * * *").
    pub schedule: Option<String>,
    /// The endpoint function to call.
    pub endpoint_name: Option<String>,
    /// The service this cron job belongs to.
    pub service_name: Option<String>,
    /// Documentation comment.
    pub doc: Option<String>,
}

impl CronJob {
    /// Creates a new CronJob.
    pub fn new(name: String, span: Span) -> Self {
        Self {
            name,
            span,
            title: None,
            every: None,
            schedule: None,
            endpoint_name: None,
            service_name: None,
            doc: None,
        }
    }
}

/// Configuration for a cron job, parsed from Python dict literal.
#[derive(Debug, Default, LitParser)]
struct DecodedCronJobConfig {
    /// Title for the cron job.
    title: Option<String>,
    /// Duration string for how often to run.
    every: Option<String>,
    /// Cron expression schedule.
    schedule: Option<String>,
}

/// Parser for cron job definitions.
pub static CRON_PARSER: ResourceParser = ResourceParser {
    name: "cron",
    interesting_pkgs: &["encoredev.cron"],
    run: parse_cron_jobs,
};

/// Intermediate struct for parsed cron job definitions.
pub(super) struct CronJobDef {
    /// Resource name (first string arg).
    pub(super) resource_name: String,
    /// Title for the cron job.
    pub(super) title: Option<String>,
    /// Duration string for how often to run.
    pub(super) every: Option<String>,
    /// Cron expression schedule.
    pub(super) schedule: Option<String>,
    /// Variable name being assigned to.
    pub(super) bind_name: Option<String>,
    /// Text range for span conversion.
    pub(super) range: TextRange,
}

impl ReferenceParser<'_> for CronJobDef {
    fn parse_resource_reference(ctx: &ReferenceContext<'_, '_>) -> ParseResult<Option<Self>> {
        // First arg must be the cron job name (string)
        let Some(resource_name) = ctx.first_string_arg() else {
            return Err(ctx.error("CronJob() requires a name as the first argument"));
        };

        // Second arg must be the config dict
        let Some(config_expr) = ctx.arg(1) else {
            return Err(ctx.error("CronJob() requires a config as the second argument"));
        };

        let config = DecodedCronJobConfig::parse_lit(config_expr)
            .map_err(|e| ctx.error_at(config_expr.range(), format!("invalid cron job config: {}", e)))?;

        Ok(Some(CronJobDef {
            resource_name,
            title: config.title,
            every: config.every,
            schedule: config.schedule,
            bind_name: ctx.bind_name.clone(),
            range: ctx.range(),
        }))
    }
}

fn parse_cron_jobs(ctx: &mut ResourceParseContext) -> ParseResult<()> {
    let names = TrackedNames::new(&[("encoredev.cron", "CronJob")]);

    let module = &ctx.module_obj.module;
    let results: Vec<ParseResult<CronJobDef>> = iter_references(
        &module.ast,
        &names,
        Some(module.mod_path.clone()),
        module.is_package,
        module.file.id,
    );

    for result in results {
        let cron_def = result?;

        let mut cron = CronJob::new(cron_def.resource_name, ctx.span(cron_def.range));
        cron.title = cron_def.title;
        cron.every = cron_def.every;
        cron.schedule = cron_def.schedule;
        cron.service_name = ctx.service_name.clone();

        let resource = Resource::CronJob(Arc::new(cron));
        ctx.add_bind(
            resource.clone(),
            BindKind::Create,
            ctx.span(cron_def.range),
            cron_def.bind_name,
        );
        ctx.add_resource(resource);
    }

    Ok(())
}
