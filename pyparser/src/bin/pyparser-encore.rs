use std::fmt::Display;
use std::io::{self, BufRead, Write};
use std::path::PathBuf;
use std::sync::Arc;

use anyhow::Result;
use encore_pyparser::ast::schema::Parser;
use prost::Message;
use serde::Deserialize;

use encore_pyparser::app::AppDesc;
use encore_pyparser::builder::{
    App, Builder, CodegenParams, CompileParams, DebugMode, ParseParams, PlainError, PrepareParams,
    TestParams,
};

fn main() -> Result<()> {
    let wd = std::env::current_dir()?;
    env_logger::init();

    let prepare = match parse_cmd()? {
        Some(Command::Prepare(prepare)) => prepare,
        Some(_) => anyhow::bail!("expected prepare command"),
        None => return Ok(()),
    };

    let app = App {
        root: prepare.app_root.clone(),
        platform_id: prepare.platform_id,
        local_id: prepare.local_id,
    };

    // let ctx = parsectx::Context {
    //     app_root: app.root.clone(),
    // };

    let builder = Builder::new(app, wd)?;
    let parser = Parser::new(&builder.ctx, &builder.loader);

    let mut parse: Option<AppDesc> = None;

    match builder.prepare(PrepareParams {}) {
        Ok(()) => {
            let json = serde_json::to_string(&())?;
            write_ok(json.as_bytes())?;
        }
        Err(err) => {
            log::error!("failed to prepare: {:?}", err);
            write_err(&err)?
        }
    }

    loop {
        let cmd = match parse_cmd()? {
            Some(cmd) => cmd,
            None => return Ok(()),
        };

        match cmd {
            Command::Prepare(_input) => {
                // Already handled above
            }

            Command::Parse(input) => {
                log::debug!("got parse input {:?}", input);
                if parse.is_some() {
                    anyhow::bail!("already parsed!");
                }

                let pp = ParseParams {
                    parse_tests: input.parse_tests,
                };

                match builder.parse(&parser, pp) {
                    Ok(result) => {
                        log::info!("parse successful");
                        write_ok(result.meta.encode_to_vec().as_slice())?;
                        parse = Some(result);
                    }
                    Err(err) => write_err(&anyhow::anyhow!(PlainError(err.to_string())))?,
                }
            }

            Command::Compile(input) => match &parse {
                None => anyhow::bail!("no parse!"),
                Some(parse_result) => {
                    let cp = CompileParams {
                        desc: parse_result,
                        debug: input.debug,
                    };

                    log::info!("starting compile");
                    match builder.compile(cp) {
                        Ok(compile) => {
                            log::info!("compile successful");
                            let json = serde_json::to_string(&compile)?;
                            write_ok(json.as_bytes())?;
                        }
                        Err(err) => {
                            log::error!("failed to compile: {:?}", err);
                            write_err(&err)?
                        }
                    };
                }
            },

            Command::Test(_input) => match &parse {
                None => anyhow::bail!("no parse!"),
                Some(parse_result) => {
                    let p = TestParams {
                        parse: parse_result,
                    };

                    match builder.test(p) {
                        Ok(test_result) => {
                            let json = serde_json::to_string(&test_result)?;
                            write_ok(json.as_bytes())?;
                        }
                        Err(err) => write_err(&err)?,
                    };
                }
            },

            Command::GenUserFacing(_input) => match &parse {
                None => anyhow::bail!("no parse!"),
                Some(parse_result) => {
                    let cp = CodegenParams { desc: parse_result };

                    log::info!("starting generate user facing code");
                    match builder.generate_code(cp) {
                        Ok(_) => write_ok(&[])?,
                        Err(err) => {
                            log::error!("failed to generate code: {:?}", err);
                            write_err(&err)?
                        }
                    };
                }
            },
        }
    }
}

fn write_data(is_ok: bool, data: &[u8]) -> io::Result<()> {
    let mut stdout = std::io::stdout().lock();
    let byte_len = ((data.len() + 1) as u32).to_le_bytes();
    stdout.write_all(&byte_len)?;
    stdout.write_all(&[if is_ok { 0 } else { 1 }])?;
    stdout.write_all(data)?;
    stdout.flush()?;
    Ok(())
}

fn write_ok(data: &[u8]) -> io::Result<()> {
    write_data(true, data)
}

fn write_err<E: Display>(err: &E) -> io::Result<()> {
    let s = err.to_string();
    write_data(false, s.as_bytes())
}

enum Command {
    Prepare(PrepareInput),
    Parse(ParseInput),
    Compile(CompileInput),
    Test(TestInput),
    GenUserFacing(GenUserFacingInput),
}

fn parse_cmd() -> Result<Option<Command>> {
    let stdin = io::stdin();
    let mut stdin = stdin.lock();

    let line = {
        let mut line = String::new();
        stdin.read_line(&mut line)?;
        line
    };

    match line.trim() {
        "" => Ok(None),
        "prepare" => {
            let mut de = serde_json::Deserializer::from_reader(stdin);
            let input = PrepareInput::deserialize(&mut de)?;
            Ok(Some(Command::Prepare(input)))
        }
        "parse" => {
            let mut de = serde_json::Deserializer::from_reader(stdin);
            let input = ParseInput::deserialize(&mut de)?;
            Ok(Some(Command::Parse(input)))
        }
        "gen-user-facing" => {
            let mut de = serde_json::Deserializer::from_reader(stdin);
            let input = GenUserFacingInput::deserialize(&mut de)?;
            Ok(Some(Command::GenUserFacing(input)))
        }
        "compile" => {
            let mut de = serde_json::Deserializer::from_reader(stdin);
            let input = CompileInput::deserialize(&mut de)?;
            Ok(Some(Command::Compile(input)))
        }
        "test" => {
            let mut de = serde_json::Deserializer::from_reader(stdin);
            let input = TestInput::deserialize(&mut de)?;
            Ok(Some(Command::Test(input)))
        }
        _ => anyhow::bail!("unknown command {:#?}", line),
    }
}

#[derive(Deserialize, Debug)]
#[allow(dead_code)]
struct PrepareInput {
    app_root: PathBuf,
    runtime_version: String,
    platform_id: Option<String>,
    local_id: String,
}

#[derive(Deserialize, Debug)]
struct ParseInput {
    parse_tests: bool,
}

#[derive(Deserialize, Debug)]
struct CompileInput {
    debug: DebugMode,
}

#[derive(Deserialize, Debug)]
struct TestInput {}

#[derive(Deserialize, Debug)]
struct GenUserFacingInput {}
