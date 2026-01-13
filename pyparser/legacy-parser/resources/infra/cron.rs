//! Cron job resource definition and parsing.

use std::sync::Arc;

use ruff_python_ast as ast;

use crate::parser::fileset::Range;
use crate::parser::resourceparser::{ResourceParseContext, ResourceParser};
use crate::parser::resources::parseutil::{get_first_string_arg, get_second_arg, is_class_call, KeywordArgs};
use crate::parser::resources::{BindKind, Resource};

/// A cron job definition.
#[derive(Debug, Clone)]
pub struct CronJob {
    /// The cron job name.
    pub name: String,
    /// Source range of the cron job definition.
    pub range: Range,
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
    /// Creates a new cron job.
    pub fn new(name: String, range: Range) -> Self {
        CronJob {
            name,
            range,
            title: None,
            every: None,
            schedule: None,
            endpoint_name: None,
            service_name: None,
            doc: None,
        }
    }
}

/// Parser for cron job definitions.
pub static CRON_PARSER: ResourceParser = ResourceParser {
    name: "cron",
    interesting_pkgs: &["encoredev.cron"],
    run: parse_cron_jobs,
};

fn parse_cron_jobs(ctx: &mut ResourceParseContext) {
    let assignments: Vec<_> = ctx.module.assignments().to_vec();

    for assignment in assignments {
        let call = match &*assignment.value {
            ast::Expr::Call(c) => c,
            _ => continue,
        };

        if !is_class_call(call, "CronJob") || !ctx.is_imported_from("CronJob", "encoredev.cron") {
            continue;
        }

        let Some(name) = get_first_string_arg(call) else {
            continue;
        };

        let mut cron = CronJob::new(name.clone(), ctx.range(assignment.range.to_text_range()));
        cron.service_name = ctx.service_name.clone();

        // Check for CronJobConfig
        if let Some(config_expr) = get_second_arg(call) {
            if let ast::Expr::Call(config_call) = config_expr {
                let kwargs = KeywordArgs::new(&config_call.arguments.keywords);

                if let Some(title) = kwargs.get_string("title") {
                    cron.title = Some(title);
                }
                if let Some(every) = kwargs.get_string("every") {
                    cron.every = Some(every);
                }
                if let Some(schedule) = kwargs.get_string("schedule") {
                    cron.schedule = Some(schedule);
                }

                // Extract endpoint function name
                if let Some(endpoint_expr) = kwargs.get("endpoint") {
                    if let ast::Expr::Name(n) = endpoint_expr {
                        cron.endpoint_name = Some(n.id.to_string());
                    }
                }
            }
        }

        let resource = Resource::CronJob(Arc::new(cron));
        ctx.add_bind(
            resource.clone(),
            BindKind::Create,
            ctx.range(assignment.range.to_text_range()),
            Some(assignment.name.clone()),
        );
        ctx.add_resource(resource);
    }
}
