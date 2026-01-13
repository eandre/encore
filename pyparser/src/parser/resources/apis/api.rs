//! API endpoint resource definition and parsing.

use std::collections::HashSet;
use std::sync::Arc;

use ruff_python_ast as ast;
use ruff_text_size::{Ranged, TextRange};

use crate::ast::loader::errors::ParseResult;
use crate::ast::loader::fileset::FilePath;
use crate::ast::loader::fileset::Span;
use crate::ast::schema::Type;
use crate::ast::scoping::references::{
    extract_string, iter_references, ReferenceContext, ReferenceParser, TrackedNames,
};
use crate::parser::resourceparser::{ResourceParseContext, ResourceParser};
use crate::parser::resources::{BindKind, Resource};

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
pub struct Endpoint<'py> {
    /// The endpoint name (function name).
    pub name: String,
    /// Source range of the endpoint definition.
    pub span: Span,
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
    pub request_type: Option<Type<'py>>,
    /// The response type (extracted from function return annotation).
    pub response_type: Option<Type<'py>>,

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

impl<'py> Endpoint<'py> {
    /// Creates a new endpoint with the given name and span, with default values.
    pub fn new(name: String, span: Span) -> Self {
        Self {
            name,
            span,
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

/// Intermediate struct for parsed endpoint definitions.
struct EndpointDef<'ast> {
    /// Function name (endpoint name).
    func_name: String,
    /// HTTP methods.
    methods: HashSet<Method>,
    /// Request path.
    path: Option<String>,
    /// Whether publicly accessible.
    expose: bool,
    /// Whether auth is required.
    auth: bool,
    /// Body size limit.
    body_limit: Option<u64>,
    /// Tags.
    tags: Vec<String>,
    /// Whether sensitive.
    sensitive: bool,
    /// Range of the function definition.
    range: TextRange,
    /// Function parameters (for request type parsing).
    func_params: &'ast ast::Parameters,
    /// Function return type annotation (for response type parsing).
    func_returns: Option<&'ast ast::Expr>,
}

impl<'ast> ReferenceParser<'ast> for EndpointDef<'ast> {
    fn parse_resource_reference(ctx: &ReferenceContext<'_, 'ast>) -> ParseResult<Option<Self>> {
        // The `api` name must be used as a decorator, not a regular call
        let Some(func) = ctx.as_decorator() else {
            // Not used as a decorator - ignore this reference
            return Ok(None);
        };

        let func_name = func.name.to_string();
        let range = func.range();
        let func_params = &func.parameters;
        let func_returns = func.returns.as_deref();

        // Parse decorator arguments if present (@api(...))
        let mut methods = HashSet::new();
        let mut path = None;
        let mut expose = false;
        let mut auth = false;
        let mut body_limit = None;
        let mut tags = Vec::new();
        let mut sensitive = false;

        if let Some(call) = ctx.as_call() {
            // Parse keyword arguments
            for kw in &call.arguments.keywords {
                let Some(arg_name) = &kw.arg else { continue };
                match arg_name.as_str() {
                    "method" => {
                        methods = parse_methods(&kw.value);
                    }
                    "path" => {
                        path = extract_string(&kw.value);
                    }
                    "expose" => {
                        if let ast::Expr::BooleanLiteral(lit) = &kw.value {
                            expose = lit.value;
                        }
                    }
                    "auth" => {
                        if let ast::Expr::BooleanLiteral(lit) = &kw.value {
                            auth = lit.value;
                        }
                    }
                    "body_limit" => {
                        if let ast::Expr::NumberLiteral(lit) = &kw.value {
                            if let ast::Number::Int(i) = &lit.value {
                                body_limit = i.as_u64();
                            }
                        }
                    }
                    "tags" => {
                        if let ast::Expr::List(list) = &kw.value {
                            tags = list.elts.iter().filter_map(|e| extract_string(e)).collect();
                        }
                    }
                    "sensitive" => {
                        if let ast::Expr::BooleanLiteral(lit) = &kw.value {
                            sensitive = lit.value;
                        }
                    }
                    _ => {}
                }
            }
        }

        Ok(Some(EndpointDef {
            func_name,
            methods,
            path,
            expose,
            auth,
            body_limit,
            tags,
            sensitive,
            range,
            func_params,
            func_returns,
        }))
    }
}

fn parse_endpoints<'py>(ctx: &mut ResourceParseContext<'py>) -> ParseResult<()> {
    let tracked = TrackedNames::new(&[("encoredev.api", "api")]);
    let module = ctx.module_obj.module;

    let results: Vec<ParseResult<EndpointDef>> = iter_references(
        &module.ast,
        &tracked,
        Some(module.mod_path.clone()),
        module.is_package,
        module.file.id,
    );

    // Derive service name from context or file path
    let service_name = match &ctx.service_name {
        Some(name) => Some(name.to_string()),
        None => match &module.file.path {
            FilePath::Real(ref buf) => buf
                .parent()
                .and_then(|p| p.file_name())
                .and_then(|s| s.to_str())
                .map(|s| s.to_string()),
            FilePath::Custom(_) => None,
        },
    };

    for result in results {
        let def = result?;

        let func_span = module.file.id.to_span(def.range);
        let mut endpoint = Endpoint::new(def.func_name.clone(), func_span);
        endpoint.endpoint_type = EndpointType::Typed;
        endpoint.service_name = service_name.clone();
        endpoint.methods = def.methods;
        endpoint.path = def.path;
        endpoint.expose = def.expose;
        endpoint.auth = def.auth;
        endpoint.body_limit = def.body_limit;
        endpoint.tags = def.tags;
        endpoint.sensitive = def.sensitive;

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

        // Parse response type from return annotation
        if let Some(returns) = def.func_returns {
            endpoint.response_type = Some(ctx.resolver.parse_type(ctx.module_obj, returns)?);
        }

        // Parse request type from first parameter's type annotation
        // Skip 'self' parameter for methods
        let params = def.func_params;
        let first_param = params
            .args
            .first()
            .filter(|p| p.parameter.name.as_str() != "self");
        if let Some(param) = first_param {
            if let Some(annotation) = &param.parameter.annotation {
                endpoint.request_type = Some(ctx.resolver.parse_type(ctx.module_obj, annotation)?);
            }
        }

        let resource = Resource::APIEndpoint(Arc::new(endpoint));
        ctx.add_bind(
            resource.clone(),
            BindKind::Create,
            func_span,
            Some(def.func_name),
        );
        ctx.add_resource(resource);
    }

    Ok(())
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
