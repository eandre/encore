//! Metrics resource definition and parsing.

use std::sync::Arc;

use ruff_text_size::TextRange;

use crate::ast::loader::errors::ParseResult;
use crate::ast::loader::fileset::Span;
use crate::ast::scoping::references::{iter_references, ReferenceContext, ReferenceParser, TrackedNames};
use crate::parser::resourceparser::{ResourceParseContext, ResourceParser};
use crate::parser::resources::{BindKind, Resource};

/// The type of metric.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MetricType {
    /// A counter (monotonically increasing value).
    Counter,
    /// A counter with labels.
    CounterGroup,
    /// A gauge (value that can go up or down).
    Gauge,
    /// A gauge with labels.
    GaugeGroup,
}

/// A metric definition.
#[derive(Debug, Clone)]
pub struct Metric {
    /// The metric name.
    pub name: String,
    /// Source range of the metric definition.
    pub span: Span,
    /// The type of metric.
    pub metric_type: MetricType,
    /// Documentation comment.
    pub doc: Option<String>,
}

impl Metric {
    /// Creates a new Metric.
    pub fn new(name: String, span: Span, metric_type: MetricType) -> Self {
        Self {
            name,
            span,
            metric_type,
            doc: None,
        }
    }
}

/// Parser for metric definitions.
pub static METRIC_PARSER: ResourceParser = ResourceParser {
    name: "metric",
    interesting_pkgs: &["encoredev.metrics"],
    run: parse_metrics,
};

/// Intermediate struct for parsed metric definitions.
pub(super) struct MetricDef {
    /// Resource name (first string arg).
    pub(super) resource_name: String,
    /// Variable name being assigned to.
    pub(super) bind_name: Option<String>,
    /// Text range for span conversion.
    pub(super) range: TextRange,
}

impl ReferenceParser<'_> for MetricDef {
    fn parse_resource_reference(ctx: &ReferenceContext<'_, '_>) -> ParseResult<Option<Self>> {
        // First arg must be the metric name (string)
        let Some(resource_name) = ctx.first_string_arg() else {
            return Err(ctx.error("Metric requires a name as the first argument"));
        };

        Ok(Some(MetricDef {
            resource_name,
            bind_name: ctx.bind_name.clone(),
            range: ctx.range(),
        }))
    }
}

fn parse_metrics(ctx: &mut ResourceParseContext) -> ParseResult<()> {
    let module = &ctx.module_obj.module;

    // Parse each metric type separately
    let metric_types = [
        ("Counter", MetricType::Counter),
        ("CounterGroup", MetricType::CounterGroup),
        ("Gauge", MetricType::Gauge),
        ("GaugeGroup", MetricType::GaugeGroup),
    ];

    for (name, metric_type) in metric_types {
        let tracked_list = [("encoredev.metrics", name)];
        let names = TrackedNames::new(&tracked_list);

        let results: Vec<ParseResult<MetricDef>> = iter_references(
            &module.ast,
            &names,
            Some(module.mod_path.clone()),
            module.is_package,
            module.file.id,
        );

        for result in results {
            let metric_def = result?;

            let metric = Metric::new(
                metric_def.resource_name,
                ctx.span(metric_def.range),
                metric_type,
            );

            let resource = Resource::Metric(Arc::new(metric));
            ctx.add_bind(
                resource.clone(),
                BindKind::Create,
                ctx.span(metric_def.range),
                metric_def.bind_name,
            );
            ctx.add_resource(resource);
        }
    }

    Ok(())
}
