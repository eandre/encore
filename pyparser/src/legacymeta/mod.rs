//! Legacy metadata computation for Python applications.
//!
//! This module computes the v1::Data metadata from parsed Python resources,
//! following the same structure as the TypeScript parser's legacymeta module.

use std::collections::HashMap;
use std::path::Path;
use std::sync::Arc;

use anyhow::Result;

use crate::ast::loader::errors::{ErrorReporter, ParseResult};
use crate::ast::loader::fileset::{FilePath, FileSet, Span};
use crate::ast::loader::ModuleId;
use crate::ast::schema::Parser;
use crate::encore::parser::meta::v1::{self, path_segment, Selector};
use crate::encore::parser::schema::v1 as schema;
use crate::parser::parser::ParsedApp;
use crate::parser::resources::apis::api::{Access, EndpointType};
use crate::parser::resources::infra::metrics::MetricType;
use crate::parser::resources::infra::pubsub_topic::DeliveryGuarantee;
use crate::parser::resources::{Bind, BindKind, Resource};
use crate::parser::service_discovery::DiscoveredService;

mod schema_conv;
pub(crate) use schema_conv::SchemaBuilder;

const DEFAULT_API_GATEWAY_NAME: &str = "api-gateway";

/// Computes legacy metadata from the parse result.
pub fn compute_meta<'a, 'py>(
    file_set: &'a FileSet,
    parse: &'a ParsedApp<'py>,
    app_root: &'a Path,
    parser: &'py Parser<'py>,
) -> ParseResult<v1::Data> {
    let schema_builder = SchemaBuilder::new(file_set, app_root, parser);
    MetaBuilder {
        file_set,
        parse,
        app_root,
        schema_builder,
        data: new_meta(),
    }
    .build()
}

struct MetaBuilder<'a, 'py> {
    file_set: &'a FileSet,
    parse: &'a ParsedApp<'py>,
    app_root: &'a Path,
    schema_builder: SchemaBuilder<'a, 'py>,
    data: v1::Data,
}

impl<'a, 'py> MetaBuilder<'a, 'py> {
    pub fn build(mut self) -> ParseResult<v1::Data> {
        self.data.app_revision = std::env::var("ENCORE_APP_REVISION").unwrap_or_default();

        let mut svc_index: HashMap<String, usize> = HashMap::new();
        let mut svc_to_pkg_index: HashMap<String, usize> = HashMap::new();

        // First, create services and packages
        for svc in &self.parse.services {
            let rel_path = svc.root.to_string_lossy().to_string();

            svc_to_pkg_index.insert(svc.name.clone(), self.data.pkgs.len());
            self.data.pkgs.push(v1::Package {
                rel_path: rel_path.clone(),
                name: svc.name.clone(),
                service_name: svc.name.clone(),
                doc: svc.doc.clone().unwrap_or_default(),
                rpc_calls: vec![],
                secrets: vec![],
                trace_nodes: vec![],
            });

            svc_index.insert(svc.name.clone(), self.data.svcs.len());
            self.data.svcs.push(v1::Service {
                name: svc.name.clone(),
                rel_path,
                rpcs: vec![],
                databases: vec![],
                buckets: vec![],
                metrics: vec![],
                has_config: false,
                migrations: vec![],
            });
        }

        // Store dependent resources for second pass
        enum Dependent<'a, 'py> {
            PubSubSubscription(&'a Bind<'py>),
            CronJob(&'a Bind<'py>),
            Gateway(&'a Bind<'py>),
        }

        let mut dependent: Vec<Dependent<'_, 'py>> = Vec::new();
        let mut topic_by_name: HashMap<String, usize> = HashMap::new();
        // Maps (module_id, bind_name) -> topic.name for subscription resolution
        let mut topic_bind_to_name: HashMap<(ModuleId, String), String> = HashMap::new();
        let mut endpoint_by_name: HashMap<(String, String), (usize, usize)> = HashMap::new();
        let mut auth_handlers: HashMap<String, Arc<crate::parser::resources::AuthHandler>> =
            HashMap::new();

        // First pass: process resources
        for bind in &self.parse.binds {
            if bind.kind != BindKind::Create {
                continue;
            }

            match &bind.resource {
                Resource::Service(_) => {
                    // Already processed above
                }

                Resource::APIEndpoint(ep) => {
                    let access_type: i32 = match ep.access {
                        Access::Private => v1::rpc::AccessType::Private as i32,
                        Access::Public => v1::rpc::AccessType::Public as i32,
                        Access::Auth => v1::rpc::AccessType::Auth as i32,
                    };

                    let http_methods: Vec<String> =
                        ep.methods.iter().map(|m| m.as_str().to_string()).collect();

                    let path = ep.path.as_ref().map(|p| parse_path(p));

                    let tags: Vec<Selector> = ep
                        .tags
                        .iter()
                        .map(|tag| Selector {
                            r#type: v1::selector::Type::Tag as i32,
                            value: tag.clone(),
                        })
                        .collect();

                    let static_assets = if ep.endpoint_type == EndpointType::Static {
                        ep.static_dir.as_ref().map(|dir| v1::rpc::StaticAssets {
                            dir_rel_path: dir.clone(),
                            not_found_rel_path: ep.not_found.clone(),
                            not_found_status: ep.not_found_status.map(|s| s as u32),
                            headers: HashMap::new(),
                        })
                    } else {
                        None
                    };

                    let (streaming_request, streaming_response) = match ep.endpoint_type {
                        EndpointType::StreamInOut => (true, true),
                        EndpointType::StreamIn => (true, false),
                        EndpointType::StreamOut => (false, true),
                        _ => (false, false),
                    };

                    // Convert request and response types to schema.
                    // For primitive types (str, int, list, dict, etc.), we use the resolved type.
                    // For complex named types (dataclass, TypedDict, etc.), the raw AST is stored
                    // in raw_request_type/raw_response_type for future type resolution when
                    // full TypeChecker integration is available.
                    let request_schema = ep
                        .request_type
                        .as_ref()
                        .and_then(|t| self.schema_builder.typ(t, ep.span).ok());
                    let response_schema = ep
                        .response_type
                        .as_ref()
                        .and_then(|t| self.schema_builder.typ(t, ep.span).ok());

                    let rpc = v1::Rpc {
                        name: ep.name.clone(),
                        doc: ep.doc.clone(),
                        service_name: ep.service_name.clone().unwrap_or_default(),
                        access_type,
                        handshake_schema: None, // Only for streaming endpoints
                        request_schema,
                        response_schema,
                        proto: if ep.endpoint_type == EndpointType::Raw {
                            v1::rpc::Protocol::Raw as i32
                        } else {
                            v1::rpc::Protocol::Regular as i32
                        },
                        path,
                        http_methods,
                        tags,
                        sensitive: ep.sensitive,
                        loc: Some(loc_from_span(self.app_root, self.file_set, ep.span)?),
                        allow_unauthenticated: !ep.auth,
                        body_limit: ep.body_limit,
                        expose: {
                            let mut map = HashMap::new();
                            if ep.expose {
                                map.insert(
                                    DEFAULT_API_GATEWAY_NAME.to_string(),
                                    v1::rpc::ExposeOptions {},
                                );
                            }
                            map
                        },
                        streaming_request,
                        streaming_response,
                        static_assets,
                    };

                    let service_name = ep.service_name.clone().unwrap_or_default();
                    if let Some(&service_idx) = svc_index.get(&service_name) {
                        let ep_idx = self.data.svcs[service_idx].rpcs.len();
                        endpoint_by_name.insert(
                            (service_name.clone(), ep.name.clone()),
                            (service_idx, ep_idx),
                        );
                        self.data.svcs[service_idx].rpcs.push(rpc);
                    }
                }

                Resource::AuthHandler(ah) => {
                    auth_handlers.insert(ah.name.clone(), ah.clone());
                }

                Resource::SQLDatabase(db) => {
                    self.data.sql_databases.push(v1::SqlDatabase {
                        name: db.name.clone(),
                        doc: db.doc.clone(),
                        migration_rel_path: db.migrations_path.clone(),
                        migrations: vec![], // TODO: Parse migration files
                        allow_non_sequential_migrations: false,
                    });
                }

                Resource::Bucket(bkt) => {
                    self.data.buckets.push(v1::Bucket {
                        name: bkt.name.clone(),
                        doc: bkt.doc.clone(),
                        versioned: bkt.versioned,
                        public: bkt.public,
                    });
                }

                Resource::PubSubTopic(topic) => {
                    let idx = self.data.pubsub_topics.len();

                    // Convert message type to schema
                    let message_type_schema =
                        self.schema_builder.typ(&topic.message_type, topic.span)?;

                    // For now, add every service as a publisher
                    let publishers: Vec<v1::pub_sub_topic::Publisher> = self
                        .parse
                        .services
                        .iter()
                        .map(|svc| v1::pub_sub_topic::Publisher {
                            service_name: svc.name.clone(),
                        })
                        .collect();

                    self.data.pubsub_topics.push(v1::PubSubTopic {
                        name: topic.name.clone(),
                        doc: topic.doc.clone(),
                        message_type: Some(message_type_schema),
                        delivery_guarantee: match topic.delivery_guarantee {
                            DeliveryGuarantee::AtLeastOnce => {
                                v1::pub_sub_topic::DeliveryGuarantee::AtLeastOnce as i32
                            }
                            DeliveryGuarantee::ExactlyOnce => {
                                v1::pub_sub_topic::DeliveryGuarantee::ExactlyOnce as i32
                            }
                        },
                        ordering_key: topic.ordering_attribute.clone().unwrap_or_default(),
                        publishers,
                        subscriptions: vec![],
                    });
                    topic_by_name.insert(topic.name.clone(), idx);

                    // Store (module_id, bind_name) -> topic.name for subscription resolution
                    if let Some(bind_name) = &bind.name {
                        topic_bind_to_name
                            .insert((bind.module_id, bind_name.clone()), topic.name.clone());
                    }
                }

                Resource::Secret(secret) => {
                    // Find the service this secret belongs to
                    if let Some(svc) = self.service_for_span(&secret.span) {
                        if let Some(&pkg_idx) = svc_to_pkg_index.get(&svc.name) {
                            self.data.pkgs[pkg_idx].secrets.push(secret.name.clone());
                        }
                    }
                }

                Resource::PubSubSubscription(_) => {
                    dependent.push(Dependent::PubSubSubscription(bind));
                }

                Resource::CronJob(_) => {
                    dependent.push(Dependent::CronJob(bind));
                }

                Resource::Gateway(_) => {
                    dependent.push(Dependent::Gateway(bind));
                }

                Resource::Metric(m) => {
                    let value_type = match m.metric_type {
                        MetricType::Counter | MetricType::CounterGroup => {
                            schema::Builtin::Int64 as i32
                        }
                        MetricType::Gauge | MetricType::GaugeGroup => {
                            schema::Builtin::Float64 as i32
                        }
                    };

                    let metric = v1::Metric {
                        name: m.name.clone(),
                        doc: m.doc.clone().unwrap_or_default(),
                        value_type,
                        service_name: None,
                        labels: vec![], // TODO: Add label support
                        kind: match m.metric_type {
                            MetricType::Counter | MetricType::CounterGroup => {
                                v1::metric::MetricKind::Counter as i32
                            }
                            MetricType::Gauge | MetricType::GaugeGroup => {
                                v1::metric::MetricKind::Gauge as i32
                            }
                        },
                    };

                    self.data.metrics.push(metric);
                }
            }
        }

        // Track seen items for deduplication
        let mut first_gateway: Option<Span> = None;

        // Second pass: process dependent resources
        for dep in &dependent {
            match dep {
                Dependent::PubSubSubscription(bind) => {
                    if let Resource::PubSubSubscription(sub) = &bind.resource {
                        // Get topic's definition location and name from the Object
                        let topic_module_id = sub.topic.module().id;
                        let topic_obj_name = sub.topic.name().to_string();

                        // Look up the topic name using (module_id, object_name)
                        let topic_name = topic_bind_to_name
                            .get(&(topic_module_id, topic_obj_name))
                            .cloned();

                        if let Some(topic_name) = topic_name {
                            if let Some(&topic_idx) = topic_by_name.get(&topic_name) {
                                let service_name = sub
                                    .service_name
                                    .clone()
                                    .or_else(|| {
                                        self.service_for_span(&sub.span).map(|s| s.name.clone())
                                    })
                                    .unwrap_or_default();

                                self.data.pubsub_topics[topic_idx].subscriptions.push(
                                    v1::pub_sub_topic::Subscription {
                                        name: sub.name.clone(),
                                        service_name,
                                        ack_deadline: sub
                                            .ack_deadline
                                            .as_ref()
                                            .map(|d| parse_duration_nanos(d))
                                            .unwrap_or(30_000_000_000), // 30s default
                                        message_retention: sub
                                            .message_retention
                                            .as_ref()
                                            .map(|d| parse_duration_nanos(d))
                                            .unwrap_or(7 * 24 * 3600_000_000_000), // 7 days default
                                        max_concurrency: sub.max_concurrency,
                                        retry_policy: Some(v1::pub_sub_topic::RetryPolicy {
                                            min_backoff: 10_000_000_000,  // 10s
                                            max_backoff: 600_000_000_000, // 10min
                                            max_retries: 100,
                                        }),
                                    },
                                );
                            }
                        }
                    }
                }

                Dependent::CronJob(bind) => {
                    if let Resource::CronJob(cj) = &bind.resource {
                        // Find the endpoint this cron job calls
                        if let Some(endpoint_name) = &cj.endpoint_name {
                            let service_name = cj
                                .service_name
                                .clone()
                                .or_else(|| self.service_for_span(&cj.span).map(|s| s.name.clone()))
                                .unwrap_or_default();

                            if let Some(&(svc_idx, _)) =
                                endpoint_by_name.get(&(service_name.clone(), endpoint_name.clone()))
                            {
                                let svc = &self.data.svcs[svc_idx];
                                let title = cj.title.clone().unwrap_or_else(|| cj.name.clone());

                                let schedule = if let Some(every) = &cj.every {
                                    format!("every:{}", parse_duration_minutes(every))
                                } else if let Some(sched) = &cj.schedule {
                                    format!("schedule:{}", sched)
                                } else {
                                    "every:60".to_string() // default to hourly
                                };

                                self.data.cron_jobs.push(v1::CronJob {
                                    id: cj.name.clone(),
                                    doc: cj.doc.clone(),
                                    title,
                                    endpoint: Some(v1::QualifiedName {
                                        pkg: svc.rel_path.clone(),
                                        name: endpoint_name.clone(),
                                    }),
                                    schedule,
                                });
                            }
                        }
                    }
                }

                Dependent::Gateway(bind) => {
                    if let Resource::Gateway(gw) = &bind.resource {
                        // Check for multiple gateways
                        if first_gateway.is_some() {
                            // Multiple gateways not yet supported - skip
                            continue;
                        }
                        first_gateway = Some(gw.span);

                        let service_name = gw
                            .service_name
                            .clone()
                            .or_else(|| self.service_for_span(&gw.span).map(|s| s.name.clone()))
                            .unwrap_or_default();

                        // Get auth handler if configured
                        let auth_handler = if gw.has_auth_handler {
                            // TODO HACK: For now, just use the first auth handler we found
                            auth_handlers.values().next().map(|ah| {
                                let loc = loc_from_span(self.app_root, self.file_set, ah.span)
                                    .unwrap_or_default();
                                let ah_service = ah
                                    .service_name
                                    .clone()
                                    .or_else(|| {
                                        self.service_for_span(&ah.span).map(|s| s.name.clone())
                                    })
                                    .unwrap_or_default();

                                v1::AuthHandler {
                                    name: ah.name.clone(),
                                    doc: ah.doc.clone().unwrap_or_default(),
                                    pkg_path: loc.pkg_path.clone(),
                                    pkg_name: loc.pkg_name.clone(),
                                    loc: Some(loc),
                                    params: None, // TODO: Add when schema parsing is implemented
                                    auth_data: None, // TODO: Add when schema parsing is implemented
                                    service_name: ah_service,
                                }
                            })
                        } else {
                            None
                        };

                        self.data.auth_handler.clone_from(&auth_handler);

                        self.data.gateways.push(v1::Gateway {
                            encore_name: DEFAULT_API_GATEWAY_NAME.to_string(),
                            explicit: Some(v1::gateway::Explicit {
                                service_name,
                                auth_handler,
                            }),
                        });
                    }
                }
            }
        }

        // Sort packages for deterministic output
        self.data.pkgs.sort_by(|a, b| a.name.cmp(&b.name));

        // Remove duplicate secrets
        for pkg in &mut self.data.pkgs {
            pkg.secrets.sort();
            pkg.secrets.dedup();
        }

        // Sort services and their contents
        for svc in &mut self.data.svcs {
            svc.databases.sort();
            svc.databases.dedup();
            svc.buckets.sort_by(|a, b| a.bucket.cmp(&b.bucket));
            svc.rpcs.sort_by(|a, b| a.name.cmp(&b.name));
        }

        // If there is no gateway, add a default one
        if self.data.gateways.is_empty() {
            self.data.gateways.push(v1::Gateway {
                encore_name: DEFAULT_API_GATEWAY_NAME.to_string(),
                explicit: None,
            });
        }

        // Collect all schema declarations
        self.data.decls = self.schema_builder.into_decls();

        Ok(self.data)
    }

    fn service_for_span(&self, span: &Span) -> Option<&DiscoveredService> {
        let file_id = span.file_id();
        let file = self.file_set.get_by_id(file_id);
        let path = file.path.as_real()?;

        self.parse
            .services
            .iter()
            .filter(|svc| {
                let svc_abs = self.app_root.join(&svc.root);
                path.starts_with(&svc_abs)
            })
            .max_by_key(|svc| svc.root.components().count())
    }
}

/// Creates a location from a span.
fn loc_from_span(app_root: &Path, file_set: &FileSet, span: Span) -> ParseResult<schema::Loc> {
    let file_id = span.file_id();
    let file = file_set.get_by_id(file_id);

    let (pkg_path, pkg_name, filename) = match &file.path {
        FilePath::Custom(s) => {
            panic!("custom file paths are unsupported");
        }
        FilePath::Real(buf) => {
            let rel_path = buf.strip_prefix(app_root).unwrap_or(buf);
            let file_name = rel_path
                .file_name()
                .map(|s| s.to_string_lossy().to_string())
                .unwrap_or_default();
            let pkg_name = rel_path
                .parent()
                .and_then(|p| p.file_name())
                .or_else(|| app_root.file_name())
                .map(|s| s.to_string_lossy().to_string())
                .unwrap_or_default();
            let pkg_path = rel_path
                .parent()
                .map(|s| s.to_string_lossy().to_string())
                .unwrap_or_else(|| ".".to_string());
            (pkg_path, pkg_name, file_name)
        }
    };

    Ok(schema::Loc {
        pkg_path,
        pkg_name,
        filename,
        start_pos: span.start() as i32,
        end_pos: span.end() as i32,
        src_line_start: 0, // TODO: Compute from file
        src_line_end: 0,
        src_col_start: 0,
        src_col_end: 0,
    })
}

/// Parses a path string into a v1::Path.
fn parse_path(path: &str) -> v1::Path {
    let segments: Vec<v1::PathSegment> = path
        .split('/')
        .filter(|s| !s.is_empty())
        .map(|segment| {
            if segment.starts_with(':') {
                // Parameter segment
                v1::PathSegment {
                    r#type: path_segment::SegmentType::Param as i32,
                    value: segment[1..].to_string(),
                    value_type: path_segment::ParamType::String as i32,
                    validation: None,
                }
            } else if segment.starts_with('*') {
                // Wildcard segment
                v1::PathSegment {
                    r#type: path_segment::SegmentType::Wildcard as i32,
                    value: segment[1..].to_string(),
                    value_type: path_segment::ParamType::String as i32,
                    validation: None,
                }
            } else if segment == "..." || segment.starts_with("...") {
                // Fallback segment
                let name = if segment.len() > 3 {
                    segment[3..].to_string()
                } else {
                    "".to_string()
                };
                v1::PathSegment {
                    r#type: path_segment::SegmentType::Fallback as i32,
                    value: name,
                    value_type: path_segment::ParamType::String as i32,
                    validation: None,
                }
            } else {
                // Literal segment
                v1::PathSegment {
                    r#type: path_segment::SegmentType::Literal as i32,
                    value: segment.to_string(),
                    value_type: path_segment::ParamType::String as i32,
                    validation: None,
                }
            }
        })
        .collect();

    v1::Path {
        r#type: v1::path::Type::Url as i32,
        segments,
    }
}

/// Parses a duration string like "1h", "30m", "1d" into nanoseconds.
fn parse_duration_nanos(s: &str) -> i64 {
    let s = s.trim();
    if s.is_empty() {
        return 0;
    }

    let (num_str, unit) = if s.ends_with("ns") {
        (&s[..s.len() - 2], "ns")
    } else if s.ends_with("us") || s.ends_with("µs") {
        (&s[..s.len() - 2], "us")
    } else if s.ends_with("ms") {
        (&s[..s.len() - 2], "ms")
    } else if s.ends_with('s') {
        (&s[..s.len() - 1], "s")
    } else if s.ends_with('m') {
        (&s[..s.len() - 1], "m")
    } else if s.ends_with('h') {
        (&s[..s.len() - 1], "h")
    } else if s.ends_with('d') {
        (&s[..s.len() - 1], "d")
    } else {
        // Assume seconds if no unit
        (s, "s")
    };

    let num: i64 = num_str.parse().unwrap_or(0);

    match unit {
        "ns" => num,
        "us" => num * 1_000,
        "ms" => num * 1_000_000,
        "s" => num * 1_000_000_000,
        "m" => num * 60 * 1_000_000_000,
        "h" => num * 3600 * 1_000_000_000,
        "d" => num * 24 * 3600 * 1_000_000_000,
        _ => 0,
    }
}

/// Parses a duration string into minutes (for cron jobs).
fn parse_duration_minutes(s: &str) -> i64 {
    let nanos = parse_duration_nanos(s);
    nanos / (60 * 1_000_000_000)
}

/// Creates a new empty metadata structure.
fn new_meta() -> v1::Data {
    v1::Data {
        module_path: "app".to_string(),
        app_revision: String::new(),
        uncommitted_changes: false,
        decls: vec![],
        pkgs: vec![],
        svcs: vec![],
        auth_handler: None,
        cron_jobs: vec![],
        pubsub_topics: vec![],
        middleware: vec![],
        cache_clusters: vec![],
        experiments: vec![],
        metrics: vec![],
        sql_databases: vec![],
        buckets: vec![],
        gateways: vec![],
        language: v1::Lang::Python as i32,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_duration_nanos() {
        assert_eq!(parse_duration_nanos("30s"), 30_000_000_000);
        assert_eq!(parse_duration_nanos("1m"), 60_000_000_000);
        assert_eq!(parse_duration_nanos("1h"), 3_600_000_000_000);
        assert_eq!(parse_duration_nanos("1d"), 86_400_000_000_000);
        assert_eq!(parse_duration_nanos("100ms"), 100_000_000);
    }

    #[test]
    fn test_parse_path() {
        let path = parse_path("/users/:id/posts");
        assert_eq!(path.segments.len(), 3);
        assert_eq!(path.segments[0].value, "users");
        assert_eq!(
            path.segments[0].r#type,
            path_segment::SegmentType::Literal as i32
        );
        assert_eq!(path.segments[1].value, "id");
        assert_eq!(
            path.segments[1].r#type,
            path_segment::SegmentType::Param as i32
        );
        assert_eq!(path.segments[2].value, "posts");
    }
}
