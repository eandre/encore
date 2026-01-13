//! Metrics resource definition and parsing.

use std::sync::Arc;

use ruff_python_ast as ast;

use crate::parser::fileset::Range;
use crate::parser::resourceparser::{ResourceParseContext, ResourceParser};
use crate::parser::resources::parseutil::get_first_string_arg;
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
    pub range: Range,
    /// The type of metric.
    pub metric_type: MetricType,
    /// Documentation comment.
    pub doc: Option<String>,
}

impl Metric {
    /// Creates a new metric.
    pub fn new(name: String, range: Range, metric_type: MetricType) -> Self {
        Metric {
            name,
            range,
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

fn parse_metrics(ctx: &mut ResourceParseContext) {
    let assignments: Vec<_> = ctx.module.assignments().to_vec();

    for assignment in assignments {
        let call = match &*assignment.value {
            ast::Expr::Call(c) => c,
            _ => continue,
        };

        let metric_type = match &*call.func {
            ast::Expr::Name(n) => {
                if ctx.is_imported_from(n.id.as_str(), "encoredev.metrics") {
                    match n.id.as_str() {
                        "Counter" => Some(MetricType::Counter),
                        "CounterGroup" => Some(MetricType::CounterGroup),
                        "Gauge" => Some(MetricType::Gauge),
                        "GaugeGroup" => Some(MetricType::GaugeGroup),
                        _ => None,
                    }
                } else {
                    None
                }
            }
            ast::Expr::Subscript(sub) => {
                // Handle generic types like CounterGroup[Labels]
                if let ast::Expr::Name(n) = &*sub.value {
                    if ctx.is_imported_from(n.id.as_str(), "encoredev.metrics") {
                        match n.id.as_str() {
                            "Counter" => Some(MetricType::Counter),
                            "CounterGroup" => Some(MetricType::CounterGroup),
                            "Gauge" => Some(MetricType::Gauge),
                            "GaugeGroup" => Some(MetricType::GaugeGroup),
                            _ => None,
                        }
                    } else {
                        None
                    }
                } else {
                    None
                }
            }
            _ => None,
        };

        let Some(mt) = metric_type else {
            continue;
        };

        let Some(name) = get_first_string_arg(call) else {
            continue;
        };

        let metric = Metric::new(name.clone(), ctx.range(assignment.range.to_text_range()), mt);

        let resource = Resource::Metric(Arc::new(metric));
        ctx.add_bind(
            resource.clone(),
            BindKind::Create,
            ctx.range(assignment.range.to_text_range()),
            Some(assignment.name.clone()),
        );
        ctx.add_resource(resource);
    }
}
