//! Integration tests for infra resource parsers.

use std::fmt::Write;
use std::path::Path;
use std::sync::Arc;

use ruff_python_parser::parse_module;

use crate::ast::loader::errors::ParseResult;
use crate::ast::loader::fileset::{File, FileId, FilePath};
use crate::ast::loader::modpath::ModulePath;
use crate::ast::loader::Module;
use crate::ast::schema::{Basic, Type};
use crate::ast::scoping::references::{iter_references, TrackedNames};

use super::cron::{CronJob, CronJobDef};
use super::metrics::{Metric, MetricDef, MetricType};
use super::objects::{Bucket, BucketDef};
use super::pubsub_topic::{DeliveryGuarantee, Topic, TopicDef};
use super::secret::{Secret, SecretDef};
use super::sqldb::{SQLDatabase, SQLDatabaseDef};

/// Helper to create a Module from source code for testing.
fn create_test_module(source: &str, mod_path_str: &str, is_package: bool) -> Module {
    let parsed =
        parse_module(source).unwrap_or_else(|e| panic!("Failed to parse test source: {:?}", e));

    let file_path = if is_package {
        FilePath::Custom(format!("{}/__init__.py", mod_path_str.replace('.', "/")))
    } else {
        FilePath::Custom(format!("{}.py", mod_path_str.replace('.', "/")))
    };

    let file = Arc::new(File::new_for_test(
        FileId::new(1),
        file_path,
        source.to_string(),
    ));

    let mod_path = ModulePath::new(mod_path_str).expect("valid module path");
    let pkg_path = if is_package {
        Some(mod_path.clone())
    } else {
        mod_path.parent()
    };

    Module {
        id: crate::ast::loader::ModuleId::new_for_test(1),
        mod_path,
        pkg_path,
        file,
        is_package,
        ast: parsed.into_syntax(),
    }
}

// ============================================================================
// Topic parsing
// ============================================================================

fn parse_topics_from_source(source: &str, mod_path: &str) -> Vec<Arc<Topic<'static>>> {
    let module = create_test_module(source, mod_path, false);
    let names = TrackedNames::new(&[("encoredev.pubsub", "Topic")]);

    let results: Vec<ParseResult<TopicDef>> = iter_references(
        &module.ast,
        &names,
        Some(module.mod_path.clone()),
        module.is_package,
        module.file.id,
    );

    results
        .into_iter()
        .filter_map(|r| match r {
            Ok(def) => Some(Arc::new(Topic {
                name: def.resource_name,
                span: module.file.id.to_span(def.range),
                delivery_guarantee: def.delivery_guarantee,
                ordering_attribute: def.ordering_attribute,
                doc: None,
                // Use Any as placeholder for tests - actual message type parsing
                // is done via the full resource parser with type resolution
                message_type: Type::Basic(Basic::Any),
            })),
            Err(e) => {
                eprintln!("Parse error: {}", e);
                None
            }
        })
        .collect()
}

fn format_topics(topics: &[Arc<Topic<'_>>]) -> String {
    let mut output = String::new();

    if topics.is_empty() {
        writeln!(output, "(no topics found)").unwrap();
        return output;
    }

    for topic in topics {
        writeln!(output, "Topic: {}", topic.name).unwrap();
        let guarantee = match topic.delivery_guarantee {
            DeliveryGuarantee::AtLeastOnce => "at-least-once",
            DeliveryGuarantee::ExactlyOnce => "exactly-once",
        };
        writeln!(output, "  delivery_guarantee: {}", guarantee).unwrap();
        if let Some(ref attr) = topic.ordering_attribute {
            writeln!(output, "  ordering_attribute: {}", attr).unwrap();
        }
        writeln!(output).unwrap();
    }

    output
}

// ============================================================================
// Secret parsing
// ============================================================================

fn parse_secrets_from_source(source: &str, mod_path: &str) -> Vec<Arc<Secret>> {
    let module = create_test_module(source, mod_path, false);
    let names = TrackedNames::new(&[
        ("encoredev.config", "secret"),
        ("encoredev.config", "Secret"),
        ("encoredev.config.secrets", "secret"),
        ("encoredev.config.secrets", "Secret"),
    ]);

    let results: Vec<ParseResult<SecretDef>> = iter_references(
        &module.ast,
        &names,
        Some(module.mod_path.clone()),
        module.is_package,
        module.file.id,
    );

    results
        .into_iter()
        .filter_map(|r| match r {
            Ok(def) => Some(Arc::new(Secret {
                name: def.resource_name,
                span: module.file.id.to_span(def.range),
                doc: None,
            })),
            Err(e) => {
                eprintln!("Parse error: {}", e);
                None
            }
        })
        .collect()
}

fn format_secrets(secrets: &[Arc<Secret>]) -> String {
    let mut output = String::new();

    if secrets.is_empty() {
        writeln!(output, "(no secrets found)").unwrap();
        return output;
    }

    for secret in secrets {
        writeln!(output, "Secret: {}", secret.name).unwrap();
        writeln!(output).unwrap();
    }

    output
}

// ============================================================================
// SQLDatabase parsing
// ============================================================================

fn parse_sqldbs_from_source(source: &str, mod_path: &str) -> Vec<Arc<SQLDatabase>> {
    let module = create_test_module(source, mod_path, false);
    let names = TrackedNames::new(&[("encoredev.storage.sqldb", "SQLDatabase")]);

    let results: Vec<ParseResult<SQLDatabaseDef>> = iter_references(
        &module.ast,
        &names,
        Some(module.mod_path.clone()),
        module.is_package,
        module.file.id,
    );

    results
        .into_iter()
        .filter_map(|r| match r {
            Ok(def) => Some(Arc::new(SQLDatabase {
                name: def.resource_name,
                span: module.file.id.to_span(def.range),
                migrations_path: def.migrations_path,
                migration_source: def.migration_source,
                doc: None,
            })),
            Err(e) => {
                eprintln!("Parse error: {}", e);
                None
            }
        })
        .collect()
}

fn format_sqldbs(dbs: &[Arc<SQLDatabase>]) -> String {
    let mut output = String::new();

    if dbs.is_empty() {
        writeln!(output, "(no databases found)").unwrap();
        return output;
    }

    for db in dbs {
        writeln!(output, "SQLDatabase: {}", db.name).unwrap();
        if let Some(ref path) = db.migrations_path {
            writeln!(output, "  migrations_path: {}", path).unwrap();
        }
        writeln!(output).unwrap();
    }

    output
}

// ============================================================================
// CronJob parsing
// ============================================================================

fn parse_crons_from_source(source: &str, mod_path: &str) -> Vec<Arc<CronJob>> {
    let module = create_test_module(source, mod_path, false);
    let names = TrackedNames::new(&[("encoredev.cron", "CronJob")]);

    let results: Vec<ParseResult<CronJobDef>> = iter_references(
        &module.ast,
        &names,
        Some(module.mod_path.clone()),
        module.is_package,
        module.file.id,
    );

    results
        .into_iter()
        .filter_map(|r| match r {
            Ok(def) => Some(Arc::new(CronJob {
                name: def.resource_name,
                span: module.file.id.to_span(def.range),
                title: def.title,
                every: def.every,
                schedule: def.schedule,
                endpoint_name: None,
                service_name: None,
                doc: None,
            })),
            Err(e) => {
                eprintln!("Parse error: {}", e);
                None
            }
        })
        .collect()
}

fn format_crons(crons: &[Arc<CronJob>]) -> String {
    let mut output = String::new();

    if crons.is_empty() {
        writeln!(output, "(no cron jobs found)").unwrap();
        return output;
    }

    for cron in crons {
        writeln!(output, "CronJob: {}", cron.name).unwrap();
        if let Some(ref title) = cron.title {
            writeln!(output, "  title: {}", title).unwrap();
        }
        if let Some(ref every) = cron.every {
            writeln!(output, "  every: {}", every).unwrap();
        }
        if let Some(ref schedule) = cron.schedule {
            writeln!(output, "  schedule: {}", schedule).unwrap();
        }
        writeln!(output).unwrap();
    }

    output
}

// ============================================================================
// Metric parsing
// ============================================================================

fn parse_metrics_from_source(source: &str, mod_path: &str) -> Vec<Arc<Metric>> {
    let module = create_test_module(source, mod_path, false);
    let mut all_metrics = Vec::new();

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
            match result {
                Ok(def) => {
                    all_metrics.push(Arc::new(Metric {
                        name: def.resource_name,
                        span: module.file.id.to_span(def.range),
                        metric_type,
                        doc: None,
                    }));
                }
                Err(e) => {
                    eprintln!("Parse error: {}", e);
                }
            }
        }
    }

    all_metrics
}

fn format_metrics(metrics: &[Arc<Metric>]) -> String {
    let mut output = String::new();

    if metrics.is_empty() {
        writeln!(output, "(no metrics found)").unwrap();
        return output;
    }

    for metric in metrics {
        writeln!(output, "Metric: {}", metric.name).unwrap();
        let type_str = match metric.metric_type {
            MetricType::Counter => "Counter",
            MetricType::CounterGroup => "CounterGroup",
            MetricType::Gauge => "Gauge",
            MetricType::GaugeGroup => "GaugeGroup",
        };
        writeln!(output, "  type: {}", type_str).unwrap();
        writeln!(output).unwrap();
    }

    output
}

// ============================================================================
// Bucket parsing
// ============================================================================

fn parse_buckets_from_source(source: &str, mod_path: &str) -> Vec<Arc<Bucket>> {
    let module = create_test_module(source, mod_path, false);
    let names = TrackedNames::new(&[("encoredev.storage.objects", "Bucket")]);

    let results: Vec<ParseResult<BucketDef>> = iter_references(
        &module.ast,
        &names,
        Some(module.mod_path.clone()),
        module.is_package,
        module.file.id,
    );

    results
        .into_iter()
        .filter_map(|r| match r {
            Ok(def) => Some(Arc::new(Bucket {
                name: def.resource_name,
                span: module.file.id.to_span(def.range),
                public: def.public,
                versioned: def.versioned,
                doc: None,
            })),
            Err(e) => {
                eprintln!("Parse error: {}", e);
                None
            }
        })
        .collect()
}

fn format_buckets(buckets: &[Arc<Bucket>]) -> String {
    let mut output = String::new();

    if buckets.is_empty() {
        writeln!(output, "(no buckets found)").unwrap();
        return output;
    }

    for bucket in buckets {
        writeln!(output, "Bucket: {}", bucket.name).unwrap();
        writeln!(output, "  public: {}", bucket.public).unwrap();
        writeln!(output, "  versioned: {}", bucket.versioned).unwrap();
        writeln!(output).unwrap();
    }

    output
}

// ============================================================================
// Generic testdata helper
// ============================================================================

fn read_testdata(filename: &str) -> String {
    let testdata_dir =
        Path::new(env!("CARGO_MANIFEST_DIR")).join("src/parser/resources/infra/testdata");
    let filepath = testdata_dir.join(filename);

    std::fs::read_to_string(&filepath)
        .unwrap_or_else(|e| panic!("Failed to read {}: {}", filepath.display(), e))
}

fn format_source(filename: &str, source: &str) -> String {
    let mut output = String::new();
    writeln!(output, "# Source: {}\n", filename).unwrap();
    writeln!(output, "```python").unwrap();
    output.push_str(source);
    if !source.ends_with('\n') {
        output.push('\n');
    }
    writeln!(output, "```\n").unwrap();
    output
}

// ============================================================================
// Topic tests
// ============================================================================

fn parse_topic_testdata(filename: &str) -> String {
    let source = read_testdata(filename);
    let module_name = filename.trim_end_matches(".py");
    let mod_path = format!("testpkg.{}", module_name);
    let topics = parse_topics_from_source(&source, &mod_path);

    let mut output = format_source(filename, &source);
    writeln!(output, "=== Parsed Topics ===\n").unwrap();
    output.push_str(&format_topics(&topics));
    output
}

#[test]
fn test_pubsub_basic() {
    let output = parse_topic_testdata("pubsub_basic.py");
    insta::assert_snapshot!(output);
}

#[test]
fn test_pubsub_scoping() {
    let output = parse_topic_testdata("pubsub_scoping.py");
    insta::assert_snapshot!(output);
}

#[test]
fn test_pubsub_aliased() {
    let output = parse_topic_testdata("pubsub_aliased.py");
    insta::assert_snapshot!(output);
}

#[test]
fn test_pubsub_wrong_module() {
    let output = parse_topic_testdata("pubsub_wrong_module.py");
    insta::assert_snapshot!(output);
}

// ============================================================================
// Secret tests
// ============================================================================

fn parse_secret_testdata(filename: &str) -> String {
    let source = read_testdata(filename);
    let module_name = filename.trim_end_matches(".py");
    let mod_path = format!("testpkg.{}", module_name);
    let secrets = parse_secrets_from_source(&source, &mod_path);

    let mut output = format_source(filename, &source);
    writeln!(output, "=== Parsed Secrets ===\n").unwrap();
    output.push_str(&format_secrets(&secrets));
    output
}

#[test]
fn test_secret_basic() {
    let output = parse_secret_testdata("secret_basic.py");
    insta::assert_snapshot!(output);
}

// ============================================================================
// SQLDatabase tests
// ============================================================================

fn parse_sqldb_testdata(filename: &str) -> String {
    let source = read_testdata(filename);
    let module_name = filename.trim_end_matches(".py");
    let mod_path = format!("testpkg.{}", module_name);
    let dbs = parse_sqldbs_from_source(&source, &mod_path);

    let mut output = format_source(filename, &source);
    writeln!(output, "=== Parsed SQLDatabases ===\n").unwrap();
    output.push_str(&format_sqldbs(&dbs));
    output
}

#[test]
fn test_sqldb_basic() {
    let output = parse_sqldb_testdata("sqldb_basic.py");
    insta::assert_snapshot!(output);
}

// ============================================================================
// CronJob tests
// ============================================================================

fn parse_cron_testdata(filename: &str) -> String {
    let source = read_testdata(filename);
    let module_name = filename.trim_end_matches(".py");
    let mod_path = format!("testpkg.{}", module_name);
    let crons = parse_crons_from_source(&source, &mod_path);

    let mut output = format_source(filename, &source);
    writeln!(output, "=== Parsed CronJobs ===\n").unwrap();
    output.push_str(&format_crons(&crons));
    output
}

#[test]
fn test_cron_basic() {
    let output = parse_cron_testdata("cron_basic.py");
    insta::assert_snapshot!(output);
}

// ============================================================================
// Metric tests
// ============================================================================

fn parse_metric_testdata(filename: &str) -> String {
    let source = read_testdata(filename);
    let module_name = filename.trim_end_matches(".py");
    let mod_path = format!("testpkg.{}", module_name);
    let metrics = parse_metrics_from_source(&source, &mod_path);

    let mut output = format_source(filename, &source);
    writeln!(output, "=== Parsed Metrics ===\n").unwrap();
    output.push_str(&format_metrics(&metrics));
    output
}

#[test]
fn test_metrics_basic() {
    let output = parse_metric_testdata("metrics_basic.py");
    insta::assert_snapshot!(output);
}

// ============================================================================
// Bucket tests
// ============================================================================

fn parse_bucket_testdata(filename: &str) -> String {
    let source = read_testdata(filename);
    let module_name = filename.trim_end_matches(".py");
    let mod_path = format!("testpkg.{}", module_name);
    let buckets = parse_buckets_from_source(&source, &mod_path);

    let mut output = format_source(filename, &source);
    writeln!(output, "=== Parsed Buckets ===\n").unwrap();
    output.push_str(&format_buckets(&buckets));
    output
}

#[test]
fn test_objects_basic() {
    let output = parse_bucket_testdata("objects_basic.py");
    insta::assert_snapshot!(output);
}

// ============================================================================
// Unit tests for specific behaviors
// ============================================================================

#[test]
fn test_topic_in_nested_function() {
    let source = r#"
from encoredev.pubsub import Topic

def outer():
    def inner():
        topic = Topic[str]("nested", {})
    return inner
"#;

    let topics = parse_topics_from_source(source, "test.module");
    assert_eq!(topics.len(), 1);
    assert_eq!(topics[0].name, "nested");
}

#[test]
fn test_topic_shadowed_in_function() {
    let source = r#"
from encoredev.pubsub import Topic

def foo():
    Topic = lambda x: x  # shadows the import
    result = Topic("not-a-topic")

# After function, Topic is still the real one
real_topic = Topic[str]("real", {})
"#;

    let topics = parse_topics_from_source(source, "test.module");
    assert_eq!(topics.len(), 1);
    assert_eq!(topics[0].name, "real");
}

#[test]
fn test_topic_with_generic_type() {
    let source = r#"
from encoredev.pubsub import Topic
from typing import TypeVar

T = TypeVar("T")

# Generic topic usage - should still be detected
typed_topic = Topic[str]("typed-events", {})
"#;

    let topics = parse_topics_from_source(source, "test.module");
    assert_eq!(topics.len(), 1);
    assert_eq!(topics[0].name, "typed-events");
}

#[test]
fn test_topic_delivery_guarantee_parsing() {
    let source = r#"
from encoredev.pubsub import Topic

at_least = Topic[str]("at-least", {"delivery_guarantee": "at-least-once"})
exactly = Topic[str]("exactly", {"delivery_guarantee": "exactly-once"})
default = Topic[str]("default", {})  # Should default to at-least-once
"#;

    let topics = parse_topics_from_source(source, "test.module");
    assert_eq!(topics.len(), 3);

    let at_least = topics.iter().find(|t| t.name == "at-least").unwrap();
    assert_eq!(at_least.delivery_guarantee, DeliveryGuarantee::AtLeastOnce);

    let exactly = topics.iter().find(|t| t.name == "exactly").unwrap();
    assert_eq!(exactly.delivery_guarantee, DeliveryGuarantee::ExactlyOnce);

    let default = topics.iter().find(|t| t.name == "default").unwrap();
    assert_eq!(default.delivery_guarantee, DeliveryGuarantee::AtLeastOnce);
}

#[test]
fn test_topic_ordering_attribute() {
    let source = r#"
from encoredev.pubsub import Topic

ordered = Topic[str]("ordered", {"ordering_attribute": "customer_id"})
unordered = Topic[str]("unordered", {})
"#;

    let topics = parse_topics_from_source(source, "test.module");
    assert_eq!(topics.len(), 2);

    let ordered = topics.iter().find(|t| t.name == "ordered").unwrap();
    assert_eq!(ordered.ordering_attribute, Some("customer_id".to_string()));

    let unordered = topics.iter().find(|t| t.name == "unordered").unwrap();
    assert_eq!(unordered.ordering_attribute, None);
}
