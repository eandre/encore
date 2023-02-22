package parseutil

import (
	"go/ast"

	"encr.dev/parser2/infra/internal/locations"
	"encr.dev/parser2/infra/resources"
	"encr.dev/parser2/internal/paths"
	"encr.dev/parser2/internal/pkginfo"
	"encr.dev/parser2/internal/schema"
)

// FindPkgNameRefs finds all references in the given package that references
// any of the given pkgNames. For each such reference it calls fn.
func FindPkgNameRefs(pkg *pkginfo.Package, pkgNames []pkginfo.QualifiedName, fn func(f *pkginfo.File, name pkginfo.QualifiedName, stack []ast.Node)) {
	// Scan files that contain the required imports.
	requiredImports := computeRequiredImports(pkgNames)
	// If the union of all imports is not a subset of the package's imports,
	// might as well not even look at individual files.
	if !hasRequiredImports(pkg.Imports, requiredImports) {
		return
	}

	// Turn cfg.Funcs into a lookup table.
	wantNames := make(map[pkginfo.QualifiedName]bool, len(pkgNames))
	for _, name := range pkgNames {
		wantNames[name] = true
	}

	for _, file := range pkg.Files {
		if !hasRequiredImports(file.Imports, requiredImports) {
			continue
		}

		// We have a file that contains the required imports.
		// Scan it for resource creation calls.
		inspector := file.ASTInspector()
		fileNames := file.Names()

		// nodeFilter are the AST types we care about inspecting.
		nodeFilter := []ast.Node{(*ast.SelectorExpr)(nil)}

		// Walk the AST to find references. Use stack information to resolve whether a particular
		// reference is in a valid location.
		inspector.WithStack(nodeFilter, func(n ast.Node, push bool, stack []ast.Node) (proceed bool) {
			// If we're popping the stack, there's nothing to do.
			if !push {
				return true
			}

			// We have a reference to a selector. See if that selector
			// references the constructors we care about.
			sel := n.(*ast.SelectorExpr) // guaranteed based on our node filter.
			name, ok := fileNames.ResolvePkgLevelRef(sel)
			if !ok {
				// Not a package-level reference.
				return true
			} else if !wantNames[name] {
				// Not a reference we care about. Keep recursing.
				return true
			}

			fn(file, name, stack)
			return true
		})
	}
}

// computeRequiredImports computes the required imports based on
// a list of func names to look for.
func computeRequiredImports(funcs []pkginfo.QualifiedName) []paths.Pkg {
	var result []paths.Pkg
	seen := make(map[paths.Pkg]bool)
	for _, f := range funcs {
		if !seen[f.PkgPath] {
			seen[f.PkgPath] = true
			result = append(result, f.PkgPath)
		}
	}
	return result
}

// hasRequiredImports reports whether the given imports set
// contains all the required imports.
func hasRequiredImports(imports map[paths.Pkg]bool, required []paths.Pkg) bool {
	for _, pkg := range required {
		if !imports[pkg] {
			return false
		}
	}
	return true
}

type ParseData struct {
	Pass         *resources.Pass
	ResourceFunc pkginfo.QualifiedName
	File         *pkginfo.File

	Stack    []ast.Node
	Call     *ast.CallExpr
	TypeArgs []schema.Type

	// TODO(andre) implement these
	Doc     string
	VarDecl *ast.GenDecl
}

type ResourceCreationSpec struct {
	AllowedLocs locations.Filter
	MinTypeArgs int
	MaxTypeArgs int
	Parse       func(ParseData) resources.Resource
}

type ReferenceData struct {
	File         *pkginfo.File
	Stack        []ast.Node
	ResourceFunc pkginfo.QualifiedName
}

func ParseResourceCreation(p *resources.Pass, spec *ResourceCreationSpec, data ReferenceData) {
	selIdx := len(data.Stack) - 1
	constructor := data.ResourceFunc

	// Classify the location the current node is contained in (meaning stack[:len(stack)-1]).
	loc := locations.Classify(data.Stack[:selIdx-1])
	if !spec.AllowedLocs.Allowed(loc) {
		// TODO make error nicer
		p.Errs.Add(data.Stack[selIdx].Pos(), "pubsub topic must be declared at the top level")
		return
	}

	// Verify the structure of the reference.

	// If we have any type arguments it will be in the parent of the selector.
	var typeArgs []schema.Type
	hasTypeArgs := spec.MinTypeArgs > 0 || spec.MaxTypeArgs > 0
	if hasTypeArgs {
		typeArgsIdx := selIdx - 1
		if typeArgsIdx < 0 {
			p.Errs.Addf(data.Stack[selIdx].Pos(), "%s requires type arguments, but none were found",
				constructor.NaiveDisplayName())
			return
		}
		args := resolveTypeArgs(data.Stack[typeArgsIdx])
		if len(args) < spec.MinTypeArgs {
			qualifier := " at least"
			if spec.MinTypeArgs == spec.MaxTypeArgs {
				qualifier = ""
			}
			p.Errs.Addf(data.Stack[selIdx].Pos(), "%s requires%s %d type argument(s), but got %d",
				constructor.NaiveDisplayName(), qualifier, spec.MinTypeArgs, len(args))
			return
		} else if len(args) > spec.MaxTypeArgs {
			qualifier := " at most"
			if spec.MinTypeArgs == spec.MaxTypeArgs {
				qualifier = ""
			}
			p.Errs.Addf(data.Stack[selIdx].Pos(), "%s requires%s %d type argument(s), but got %d",
				constructor.NaiveDisplayName(), qualifier, spec.MaxTypeArgs, len(args))
		}
		for _, arg := range args {
			typeArgs = append(typeArgs, p.SchemaParser.ParseType(data.File, arg))
		}
	}

	// Make sure the reference is called
	callIdx := selIdx - 1
	if hasTypeArgs {
		callIdx--
	}
	if callIdx < 0 {
		p.Errs.Addf(data.Stack[selIdx].Pos(), "%s cannot be referenced without being called",
			constructor.NaiveDisplayName())
		return
	}
	call, ok := data.Stack[callIdx].(*ast.CallExpr)
	if !ok {
		p.Errs.Addf(data.Stack[selIdx].Pos(), "%s cannot be referenced without being called",
			constructor.NaiveDisplayName())
		return
	}

	spec.Parse(ParseData{
		Pass:         p,
		File:         data.File,
		Stack:        data.Stack,
		Call:         call,
		TypeArgs:     typeArgs,
		ResourceFunc: data.ResourceFunc,
	})
}

// resolveTypeArgs resolves the type argument expressions for the given node.
func resolveTypeArgs(node ast.Node) []ast.Expr {
	switch n := node.(type) {
	case *ast.IndexExpr:
		return []ast.Expr{n.Index}
	case *ast.IndexListExpr:
		return n.Indices
	}
	return nil
}