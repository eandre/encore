use std::{os, path::PathBuf};

use encore_pyparser::{
    ast::schema::Parser,
    builder::{App, Builder, ParseParams},
};

fn main() {
    env_logger::init();

    // Read the app root from the first arg.
    let app_root = PathBuf::from(std::env::args().nth(1).expect("missing app root"));

    let app = App {
        root: app_root.clone(),
        platform_id: None,
        local_id: "test".to_string(),
    };
    let wd = std::env::current_dir().expect("unable to get current working directory");

    let builder = Builder::new(app, wd).expect("unable to construct builder");

    let parser = Parser::new(&builder.ctx, &builder.loader);
    let pp = ParseParams { parse_tests: false };

    match builder.parse(&parser, pp) {
        Ok(desc) => {
            println!("Successfully parsed {}", app_root.display());
            println!("  Services: {}", desc.meta.svcs.len());
            for svc in &desc.meta.svcs {
                println!("    - {} ({} endpoints)", svc.name, svc.rpcs.len());
            }
            println!("  PubSub Topics: {}", desc.meta.pubsub_topics.len());
            println!("  SQL Databases: {}", desc.meta.sql_databases.len());
            println!("  Buckets: {}", desc.meta.buckets.len());
            println!("  Cron Jobs: {}", desc.meta.cron_jobs.len());
            println!("  Metrics: {}", desc.meta.metrics.len());
        }
        Err(err) => {
            eprintln!("Parse error: {}", err);
            std::process::exit(1);
        }
    }
}
