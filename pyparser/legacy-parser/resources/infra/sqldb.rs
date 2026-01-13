//! SQL database resource definition and parsing.

use std::sync::Arc;

use ruff_python_ast as ast;

use crate::parser::fileset::Range;
use crate::parser::resourceparser::{ResourceParseContext, ResourceParser};
use crate::parser::resources::parseutil::{
    get_first_string_arg, get_second_arg, is_class_call, KeywordArgs,
};
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
    pub range: Range,
    /// Path to migrations directory.
    pub migrations_path: Option<String>,
    /// Migration source type.
    pub migration_source: Option<MigrationSource>,
    /// Documentation comment.
    pub doc: Option<String>,
}

impl SQLDatabase {
    /// Creates a new SQL database.
    pub fn new(name: String, range: Range) -> Self {
        SQLDatabase {
            name,
            range,
            migrations_path: None,
            migration_source: None,
            doc: None,
        }
    }
}

/// Parser for SQL database definitions.
pub static SQLDB_PARSER: ResourceParser = ResourceParser {
    name: "sqldb",
    interesting_pkgs: &["encoredev.storage.sqldb"],
    run: parse_sql_databases,
};

fn parse_sql_databases(ctx: &mut ResourceParseContext) {
    let assignments: Vec<_> = ctx.module.assignments().to_vec();

    for assignment in assignments {
        let call = match &*assignment.value {
            ast::Expr::Call(c) => c,
            _ => continue,
        };

        if !is_class_call(call, "SQLDatabase") {
            continue;
        }

        if !ctx.is_imported_from("SQLDatabase", "encoredev.storage.sqldb") {
            continue;
        }

        let Some(name) = get_first_string_arg(call) else {
            continue;
        };

        let mut db = SQLDatabase::new(name.clone(), ctx.range(assignment.range.to_text_range()));

        // Check for SQLDatabaseConfig
        if let Some(config_expr) = get_second_arg(call) {
            if let ast::Expr::Call(config_call) = config_expr {
                let kwargs = KeywordArgs::new(&config_call.arguments.keywords);

                // migrations can be a string or SQLMigrationsConfig
                if let Some(migrations_expr) = kwargs.get("migrations") {
                    match migrations_expr {
                        ast::Expr::StringLiteral(lit) => {
                            db.migrations_path = Some(lit.value.to_string());
                        }
                        ast::Expr::Call(mig_call) => {
                            // SQLMigrationsConfig(path="...", source="...")
                            let mig_kwargs = KeywordArgs::new(&mig_call.arguments.keywords);
                            if let Some(path) = mig_kwargs.get_string("path") {
                                db.migrations_path = Some(path);
                            }
                            if let Some(source) = mig_kwargs.get_string("source") {
                                db.migration_source = match source.as_str() {
                                    "prisma" => Some(MigrationSource::Prisma),
                                    "drizzle" => Some(MigrationSource::Drizzle),
                                    "drizzle/v1" => Some(MigrationSource::DrizzleV1),
                                    _ => Some(MigrationSource::Standard),
                                };
                            }
                        }
                        _ => {}
                    }
                }
            }
        }

        let resource = Resource::SQLDatabase(Arc::new(db));
        ctx.add_bind(
            resource.clone(),
            BindKind::Create,
            ctx.range(assignment.range.to_text_range()),
            Some(assignment.name.clone()),
        );
        ctx.add_resource(resource);
    }
}
