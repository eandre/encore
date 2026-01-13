//! SQL database resource definition and parsing.

use std::sync::Arc;

use pylitparser::LitParser;
use pylitparser_derive::LitParser;
use ruff_text_size::{Ranged, TextRange};

use crate::ast::loader::errors::ParseResult;
use crate::ast::loader::fileset::Span;
use crate::ast::scoping::references::{iter_references, ReferenceContext, ReferenceParser, TrackedNames};
use crate::parser::resourceparser::{ResourceParseContext, ResourceParser};
use crate::parser::resources::{BindKind, Resource};

/// Migration source type.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MigrationSource {
    /// Standard SQL migrations.
    Standard,
    /// Prisma migrations.
    Prisma,
    /// Drizzle migrations.
    Drizzle,
    /// Drizzle v1 migrations.
    DrizzleV1,
}

/// A SQL database definition.
#[derive(Debug, Clone)]
pub struct SQLDatabase {
    /// The database name.
    pub name: String,
    /// Source range of the database definition.
    pub span: Span,
    /// Path to migrations directory.
    pub migrations_path: Option<String>,
    /// Migration source type.
    pub migration_source: Option<MigrationSource>,
    /// Documentation comment.
    pub doc: Option<String>,
}

impl SQLDatabase {
    /// Creates a new SQLDatabase.
    pub fn new(name: String, span: Span) -> Self {
        Self {
            name,
            span,
            migrations_path: None,
            migration_source: None,
            doc: None,
        }
    }
}

/// Configuration for a SQL database, parsed from Python dict literal.
#[derive(Debug, Default, LitParser)]
struct DecodedSQLDatabaseConfig {
    /// Path to migrations directory.
    migrations: Option<String>,
}

/// Parser for SQL database definitions.
pub static SQLDB_PARSER: ResourceParser = ResourceParser {
    name: "sqldb",
    interesting_pkgs: &["encoredev.storage.sqldb"],
    run: parse_sql_databases,
};

/// Intermediate struct for parsed SQL database definitions.
pub(super) struct SQLDatabaseDef {
    /// Resource name (first string arg).
    pub(super) resource_name: String,
    /// Path to migrations directory.
    pub(super) migrations_path: Option<String>,
    /// Migration source type.
    pub(super) migration_source: Option<MigrationSource>,
    /// Variable name being assigned to.
    pub(super) bind_name: Option<String>,
    /// Text range for span conversion.
    pub(super) range: TextRange,
}

impl ReferenceParser<'_> for SQLDatabaseDef {
    fn parse_resource_reference(ctx: &ReferenceContext<'_, '_>) -> ParseResult<Option<Self>> {
        // First arg must be the database name (string)
        let Some(resource_name) = ctx.first_string_arg() else {
            return Err(ctx.error("SQLDatabase() requires a name as the first argument"));
        };

        // Second arg must be the config dict
        let Some(config_expr) = ctx.arg(1) else {
            return Err(ctx.error("SQLDatabase() requires a config as the second argument"));
        };

        let config = DecodedSQLDatabaseConfig::parse_lit(config_expr)
            .map_err(|e| ctx.error_at(config_expr.range(), format!("invalid database config: {}", e)))?;

        // Parse migrations - can be a string path or a nested config
        let (migrations_path, migration_source) = parse_migrations_value(config.migrations.as_deref());

        Ok(Some(SQLDatabaseDef {
            resource_name,
            migrations_path,
            migration_source,
            bind_name: ctx.bind_name.clone(),
            range: ctx.range(),
        }))
    }
}

/// Parse the migrations value which can be a simple path string.
fn parse_migrations_value(migrations: Option<&str>) -> (Option<String>, Option<MigrationSource>) {
    match migrations {
        Some(path) => (Some(path.to_string()), Some(MigrationSource::Standard)),
        None => (None, None),
    }
}

fn parse_sql_databases(ctx: &mut ResourceParseContext) -> ParseResult<()> {
    let names = TrackedNames::new(&[("encoredev.storage.sqldb", "SQLDatabase")]);

    let module = &ctx.module_obj.module;
    let results: Vec<ParseResult<SQLDatabaseDef>> = iter_references(
        &module.ast,
        &names,
        Some(module.mod_path.clone()),
        module.is_package,
        module.file.id,
    );

    for result in results {
        let db_def = result?;

        let mut db = SQLDatabase::new(db_def.resource_name, ctx.span(db_def.range));
        db.migrations_path = db_def.migrations_path;
        db.migration_source = db_def.migration_source;

        let resource = Resource::SQLDatabase(Arc::new(db));
        ctx.add_bind(
            resource.clone(),
            BindKind::Create,
            ctx.span(db_def.range),
            db_def.bind_name,
        );
        ctx.add_resource(resource);
    }

    Ok(())
}
