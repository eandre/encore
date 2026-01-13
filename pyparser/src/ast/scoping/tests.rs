//! Snapshot tests for scope analysis.

use std::fmt::Write;
use std::path::Path;

use ruff_python_parser::parse_module;

use super::*;
use crate::ast::loader::modpath::ModulePath;
use crate::ast::visitor::Path as VisitorPath;

/// Formats the scope analysis results for snapshot testing.
fn format_scope_analysis(tracker: &ScopeTracker) -> String {
    let mut output = String::new();

    writeln!(output, "=== Scope Analysis ===\n").unwrap();

    if let Some(mp) = tracker.module_path() {
        writeln!(output, "Module: {}", mp.as_str()).unwrap();
    }
    if let Some(pp) = tracker.pkg_path() {
        writeln!(output, "Package: {}", pp.as_str()).unwrap();
    }
    writeln!(output).unwrap();

    // Output all scopes and their bindings
    for (idx, scope) in tracker.scopes.iter().enumerate() {
        let kind = match scope.kind {
            ScopeKind::Module => "Module",
            ScopeKind::Function => "Function",
            ScopeKind::Class => "Class",
            ScopeKind::Comprehension => "Comprehension",
        };

        writeln!(output, "Scope {} ({}):", idx, kind).unwrap();

        // Collect and sort bindings for deterministic output
        let mut bindings: Vec<_> = scope.bindings().collect();
        bindings.sort_by(|a, b| a.0.cmp(b.0));

        if bindings.is_empty() {
            writeln!(output, "  (no bindings)").unwrap();
        } else {
            for (name, binding) in bindings {
                let kind_str = match binding.kind {
                    BindingKind::Local => "local",
                    BindingKind::Parameter => "param",
                    BindingKind::Import => "import",
                    BindingKind::LoopVar => "loop",
                    BindingKind::ExceptVar => "except",
                    BindingKind::WithVar => "with",
                    BindingKind::Global => "global",
                    BindingKind::Nonlocal => "nonlocal",
                };

                if let Some(ref import_from) = binding.import_from {
                    writeln!(output, "  {} ({}) from {}", name, kind_str, import_from.as_str())
                        .unwrap();
                } else {
                    writeln!(output, "  {} ({})", name, kind_str).unwrap();
                }
            }
        }

        // Show global/nonlocal declarations
        if !scope.globals.is_empty() {
            let mut globals: Vec<_> = scope.globals.iter().map(|s| s.as_str()).collect();
            globals.sort();
            writeln!(output, "  [global declarations: {}]", globals.join(", ")).unwrap();
        }
        if !scope.nonlocals.is_empty() {
            let mut nonlocals: Vec<_> = scope.nonlocals.iter().map(|s| s.as_str()).collect();
            nonlocals.sort();
            writeln!(output, "  [nonlocal declarations: {}]", nonlocals.join(", ")).unwrap();
        }

        writeln!(output).unwrap();
    }

    output
}

/// Analyzes a Python source file and returns formatted scope information.
fn analyze_file(source: &str, module_path: Option<&str>, is_package: bool) -> String {
    let parsed = parse_module(source).expect("Failed to parse Python source");

    let mp = module_path.and_then(ModulePath::new);
    let mut visitor = ScopeVisitor::new(mp, is_package);

    let mut path = VisitorPath::new();
    visitor.visit_module(parsed.syntax(), &mut path);

    format_scope_analysis(&visitor.tracker)
}

/// Analyzes a Python source file from the testdata directory.
fn analyze_testdata(filename: &str) -> String {
    let testdata_dir = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("src/ast/scoping/testdata");
    let filepath = testdata_dir.join(filename);

    let source = std::fs::read_to_string(&filepath)
        .unwrap_or_else(|e| panic!("Failed to read {}: {}", filepath.display(), e));

    // Derive module path from filename
    let module_name = filename.trim_end_matches(".py");
    let module_path = format!("testpkg.{}", module_name);

    let mut output = String::new();
    writeln!(output, "# Source: {}\n", filename).unwrap();
    writeln!(output, "```python").unwrap();
    output.push_str(&source);
    if !source.ends_with('\n') {
        output.push('\n');
    }
    writeln!(output, "```\n").unwrap();

    output.push_str(&analyze_file(&source, Some(&module_path), false));

    output
}

#[test]
fn test_basic_scoping() {
    let output = analyze_testdata("basic_scoping.py");
    insta::assert_snapshot!(output);
}

#[test]
fn test_class_scoping() {
    let output = analyze_testdata("class_scoping.py");
    insta::assert_snapshot!(output);
}

#[test]
fn test_comprehensions() {
    let output = analyze_testdata("comprehensions.py");
    insta::assert_snapshot!(output);
}

#[test]
fn test_global_nonlocal() {
    let output = analyze_testdata("global_nonlocal.py");
    insta::assert_snapshot!(output);
}

#[test]
fn test_imports() {
    let output = analyze_testdata("imports.py");
    insta::assert_snapshot!(output);
}

#[test]
fn test_closures() {
    let output = analyze_testdata("closures.py");
    insta::assert_snapshot!(output);
}

#[test]
fn test_lambda() {
    let output = analyze_testdata("lambda.py");
    insta::assert_snapshot!(output);
}

#[test]
fn test_walrus() {
    let output = analyze_testdata("walrus.py");
    insta::assert_snapshot!(output);
}

#[test]
fn test_control_flow() {
    let output = analyze_testdata("control_flow.py");
    insta::assert_snapshot!(output);
}

// Additional unit tests for specific behaviors

#[test]
fn test_relative_import_from_package() {
    let source = r#"
from . import sibling
from .sub import thing
from ..parent import other
"#;

    let output = analyze_file(source, Some("myapp.services"), true);
    insta::assert_snapshot!(output);
}

#[test]
fn test_relative_import_from_module() {
    let source = r#"
from . import sibling
from .sub import thing
from ..parent import other
"#;

    let output = analyze_file(source, Some("myapp.services.api"), false);
    insta::assert_snapshot!(output);
}

#[test]
fn test_class_method_cannot_see_class_vars() {
    let source = r#"
class Foo:
    x = 1

    def method(self):
        # x here would be an error - class scope not enclosing
        y = 2
        return y
"#;

    // Parse and analyze
    let parsed = parse_module(source).expect("parse");
    let mut visitor = ScopeVisitor::new(None, false);
    let mut path = VisitorPath::new();
    visitor.visit_module(parsed.syntax(), &mut path);

    // When in the method, x should NOT be found (class scope skipped)
    // We can verify by checking the tracker after visiting
    // The method scope should only have 'y' and 'self' (parameter)

    let output = format_scope_analysis(&visitor.tracker);
    insta::assert_snapshot!(output);
}

#[test]
fn test_walrus_escapes_comprehension() {
    let source = r#"
# y should be bound in module scope, not comprehension
result = [y := x * 2 for x in range(5)]
# y is accessible here
z = y
"#;

    let output = analyze_file(source, None, false);
    insta::assert_snapshot!(output);
}

#[test]
fn test_nested_functions_with_nonlocal() {
    let source = r#"
def outer():
    x = 1

    def middle():
        y = 2

        def inner():
            nonlocal x, y
            x = 10
            y = 20

        inner()
        return x, y

    return middle()
"#;

    let output = analyze_file(source, None, false);
    insta::assert_snapshot!(output);
}
