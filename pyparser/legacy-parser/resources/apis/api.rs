//! API endpoint resource definition and parsing.

use std::collections::HashSet;
use std::sync::Arc;

use ruff_python_ast as ast;

use crate::parser::fileset::Range;
use crate::parser::resourceparser::{ResourceParseContext, ResourceParser};
use crate::parser::resources::parseutil::{
    extract_string, get_decorator_call, is_decorator_call, KeywordArgs,
};
use crate::parser::resources::{BindKind, Resource};
use crate::parser::types::{ModuleId as TypeModuleId, Type, TypeChecker};
use crate::parser::FilePath;

/// HTTP method.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Method {
    GET,
    POST,
    PUT,
    PATCH,
    DELETE,
    HEAD,
    OPTIONS,
    TRACE,
    CONNECT,
}

impl Method {
    /// Parses a method from a string.
    pub fn from_str(s: &str) -> Option<Self> {
        match s.to_uppercase().as_str() {
            "GET" => Some(Method::GET),
            "POST" => Some(Method::POST),
            "PUT" => Some(Method::PUT),
            "PATCH" => Some(Method::PATCH),
            "DELETE" => Some(Method::DELETE),
            "HEAD" => Some(Method::HEAD),
            "OPTIONS" => Some(Method::OPTIONS),
            "TRACE" => Some(Method::TRACE),
            "CONNECT" => Some(Method::CONNECT),
            _ => None,
        }
    }

    /// Returns the method as a string.
    pub fn as_str(&self) -> &'static str {
        match self {
            Method::GET => "GET",
            Method::POST => "POST",
            Method::PUT => "PUT",
            Method::PATCH => "PATCH",
            Method::DELETE => "DELETE",
            Method::HEAD => "HEAD",
            Method::OPTIONS => "OPTIONS",
            Method::TRACE => "TRACE",
            Method::CONNECT => "CONNECT",
        }
    }
}

/// The type of API endpoint.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum EndpointType {
    /// A regular typed API endpoint.
    Typed,
    /// A raw endpoint (receives raw HTTP request/response).
    Raw,
    /// A static file serving endpoint.
    Static,
    /// A bidirectional streaming endpoint.
    StreamInOut,
    /// An input streaming endpoint.
    StreamIn,
    /// An output streaming endpoint.
    StreamOut,
}

/// Access type for the endpoint.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Access {
    /// Public access (exposed via gateway).
    Public,
    /// Private access (internal service-to-service only).
    Private,
    /// Requires authentication.
    Auth,
}

/// An API endpoint definition.
#[derive(Debug, Clone)]
pub struct Endpoint {
    /// The endpoint name (function name).
    pub name: String,
    /// Source range of the endpoint definition.
    pub range: Range,
    /// The type of endpoint.
    pub endpoint_type: EndpointType,
    /// HTTP methods this endpoint responds to.
    pub methods: HashSet<Method>,
    /// The request path (e.g., "/users/:id").
    pub path: Option<String>,
    /// Whether this endpoint is publicly accessible.
    pub expose: bool,
    /// Whether authentication is required.
    pub auth: bool,
    /// The access type.
    pub access: Access,
    /// Maximum request body size in bytes.
    pub body_limit: Option<u64>,
    /// Tags for filtering.
    pub tags: Vec<String>,
    /// Whether request data should be excluded from traces.
    pub sensitive: bool,
    /// Documentation comment.
    pub doc: Option<String>,
    /// The service this endpoint belongs to.
    pub service_name: Option<String>,

    // Request/Response types
    /// The request type (extracted from function parameters).
    pub request_type: Option<Type>,
    /// The response type (extracted from function return annotation).
    pub response_type: Option<Type>,

    // For static endpoints
    /// The directory containing static files.
    pub static_dir: Option<String>,
    /// The file to serve when a requested file is not found.
    pub not_found: Option<String>,
    /// HTTP status code for not found fallback.
    pub not_found_status: Option<u16>,
    /// Custom headers for static files.
    pub static_headers: Option<Vec<(String, String)>>,
}

impl Endpoint {
    /// Creates a new endpoint with default values.
    pub fn new(name: String, range: Range) -> Self {
        Endpoint {
            name,
            range,
            endpoint_type: EndpointType::Typed,
            methods: HashSet::new(),
            path: None,
            expose: false,
            auth: false,
            access: Access::Private,
            body_limit: None,
            tags: Vec::new(),
            sensitive: false,
            doc: None,
            service_name: None,
            request_type: None,
            response_type: None,
            static_dir: None,
            not_found: None,
            not_found_status: None,
            static_headers: None,
        }
    }

    /// Returns whether this endpoint responds to any method.
    pub fn responds_to_all(&self) -> bool {
        self.methods.is_empty()
    }
}

/// Parser for API endpoint definitions.
pub static ENDPOINT_PARSER: ResourceParser = ResourceParser {
    name: "endpoint",
    interesting_pkgs: &["encoredev.api"],
    run: parse_endpoints,
};

fn parse_endpoints(ctx: &mut ResourceParseContext) {
    let module = ctx.module.clone();

    // Collect function defs first
    let func_defs: Vec<_> = ctx
        .module
        .ast
        .body
        .iter()
        .filter_map(|stmt| {
            if let ast::Stmt::FunctionDef(func) = stmt {
                Some(func.clone())
            } else {
                None
            }
        })
        .collect();

    let service_name = match &ctx.service_name {
        Some(name) => Some(name.to_string()),
        None => {
            // Derive from file path
            match &module.file_path {
                FilePath::Real(ref buf) => buf
                    .parent()
                    .and_then(|p| p.file_name())
                    .and_then(|s| s.to_str())
                    .map(|s| s.to_string()),
                FilePath::Custom(_) => None,
            }
        }
    };

    // Look for decorated functions: @api or @api(...)
    for func in func_defs {
        for decorator in &func.decorator_list {
            // Check for @api or @api(...)
            if !is_decorator_call(decorator, "api") || !ctx.is_imported_from("api", "encoredev.api")
            {
                continue;
            }

            let mut endpoint = Endpoint::new(func.name.to_string(), ctx.range(func.range));
            endpoint.endpoint_type = EndpointType::Typed;
            endpoint.service_name = service_name.clone();

            // Parse decorator arguments if it's a call
            if let Some(call) = get_decorator_call(decorator) {
                let kwargs = KeywordArgs::new(&call.arguments.keywords);

                // Parse method(s)
                if let Some(method_expr) = kwargs.get("method") {
                    endpoint.methods = parse_methods(method_expr);
                }

                // Parse other options
                if let Some(path) = kwargs.get_string("path") {
                    endpoint.path = Some(path);
                }
                if let Some(expose) = kwargs.get_bool("expose") {
                    endpoint.expose = expose;
                }
                if let Some(auth) = kwargs.get_bool("auth") {
                    endpoint.auth = auth;
                }
                if let Some(limit) = kwargs.get_int("body_limit") {
                    endpoint.body_limit = Some(limit as u64);
                }
                if let Some(tags) = kwargs.get_string_list("tags") {
                    endpoint.tags = tags;
                }
                if let Some(sensitive) = kwargs.get_bool("sensitive") {
                    endpoint.sensitive = sensitive;
                }
            }

            // Determine access type
            endpoint.access = if endpoint.auth {
                Access::Auth
            } else if endpoint.expose {
                Access::Public
            } else {
                Access::Private
            };

            // Default to POST if no methods specified
            if endpoint.methods.is_empty() {
                endpoint.methods.insert(Method::POST);
            }

            // Extract request and response types using TypeChecker
            let type_checker = ctx.type_checker();
            let module_id = ctx.type_module_id();

            // Extract request type from function parameters
            endpoint.request_type = extract_request_type(&func.parameters, &type_checker, module_id);

            // Extract response type from return annotation
            if let Some(ret) = func.returns.as_ref() {
                endpoint.response_type = Some(type_checker.resolve_type(module_id, ret));
            }

            let resource = Resource::APIEndpoint(Arc::new(endpoint));
            ctx.add_bind(
                resource.clone(),
                BindKind::Create,
                ctx.range(func.range),
                Some(func.name.to_string()),
            );
            ctx.add_resource(resource);

            break;
        }
    }
}

/// Extract request type from function parameters.
/// Expects a single request parameter (excluding 'self') with an interface type annotation.
fn extract_request_type(
    params: &ast::Parameters,
    type_checker: &TypeChecker,
    module_id: TypeModuleId,
) -> Option<Type> {
    // Get the first non-self parameter
    let request_param = params.args.iter().find(|p| {
        let name = p.parameter.name.as_str();
        name != "self"
    })?;

    // Get the type annotation
    let annotation = request_param.parameter.annotation.as_ref()?;

    // Resolve the type using TypeChecker
    Some(type_checker.resolve_type(module_id, annotation))
}

fn parse_methods(expr: &ast::Expr) -> HashSet<Method> {
    let mut methods = HashSet::new();

    match expr {
        ast::Expr::StringLiteral(lit) => {
            let s = lit.value.to_string();
            if s == "*" {
                // All methods
                methods.insert(Method::GET);
                methods.insert(Method::POST);
                methods.insert(Method::PUT);
                methods.insert(Method::PATCH);
                methods.insert(Method::DELETE);
                methods.insert(Method::HEAD);
                methods.insert(Method::OPTIONS);
            } else if let Some(m) = Method::from_str(&s) {
                methods.insert(m);
            }
        }
        ast::Expr::List(list) => {
            for elem in &list.elts {
                if let Some(s) = extract_string(elem) {
                    if let Some(m) = Method::from_str(&s) {
                        methods.insert(m);
                    }
                }
            }
        }
        _ => {}
    }

    methods
}
