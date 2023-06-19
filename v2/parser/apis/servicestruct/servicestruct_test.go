package servicestruct

import (
	"go/ast"
	"go/token"
	"strconv"
	"testing"

	qt "github.com/frankban/quicktest"
	"github.com/google/go-cmp/cmp"
	"github.com/google/go-cmp/cmp/cmpopts"
	"github.com/rogpeppe/go-internal/txtar"

	"encr.dev/pkg/option"
	"encr.dev/pkg/paths"
	"encr.dev/v2/internals/pkginfo"
	"encr.dev/v2/internals/protoparse"
	"encr.dev/v2/internals/schema"
	"encr.dev/v2/internals/testutil"
	"encr.dev/v2/parser/apis/internal/directive"
)

func TestParseServiceStruct(t *testing.T) {
	type testCase struct {
		name     string
		imports  []string
		def      string
		want     *ServiceStruct
		wantErrs []string
	}
	file := fileForPkg("foo", "example.com")
	tests := []testCase{
		{
			name: "basic",
			def: `
//encore:service
type Foo struct {}
`,
			want: &ServiceStruct{
				Decl: &schema.TypeDecl{
					File:       file,
					Name:       "Foo",
					Type:       schema.StructType{},
					TypeParams: nil,
				},
			},
		},
		{
			name: "with_grpc_pkgpath",
			def: `
//encore:service grpc=path.to.grpc.Service
type Foo struct {}
-- proto/path/to/grpc.proto --
syntax = "proto3";
package path.to.grpc;

option go_package = "example.com/generated";

service Service {
	rpc Bar (BarRequest) returns (BarResponse);
}
message BarRequest {}
message BarResponse {}
-- generated/generated.go --
package generated
`,
			want: &ServiceStruct{
				Decl: &schema.TypeDecl{
					File:       file,
					Name:       "Foo",
					Type:       schema.StructType{},
					TypeParams: nil,
				},
			},
		},
		{
			name: "with_grpc_filepath",
			def: `
//encore:service grpc=path/to/grpc.proto:Service
type Foo struct {}
-- proto/path/to/grpc.proto --
syntax = "proto3";

option go_package = "example.com/generated";

package path.to.grpc;

service Service {
	rpc Bar (BarRequest) returns (BarResponse);
}
message BarRequest {}
message BarResponse {}
-- generated/generated.go --
package generated
`,
			want: &ServiceStruct{
				Decl: &schema.TypeDecl{
					File:       file,
					Name:       "Foo",
					Type:       schema.StructType{},
					TypeParams: nil,
				},
			},
		},
		{
			name: "with_init_func",
			def: `
//encore:service
type Foo struct {}
func initFoo() (*Foo, error) {}
`,
			want: &ServiceStruct{
				Decl: &schema.TypeDecl{
					File:       file,
					Name:       "Foo",
					Type:       schema.StructType{},
					TypeParams: nil,
				},
				Init: option.Some(&schema.FuncDecl{
					Name: "initFoo",
					Type: schema.FuncType{
						Results: []schema.Param{
							{Type: schema.PointerType{Elem: schema.NamedType{
								DeclInfo: &pkginfo.PkgDeclInfo{
									Name: "Foo",
									Type: token.TYPE,
								},
							}}},
							{Type: schema.BuiltinType{Kind: schema.Error}},
						},
					},
				}),
			},
		},
		{
			name: "error_init_no_service",
			def: `
//encore:service
type Foo struct {}
func initFoo() error {}
`,
			wantErrs: []string{`.*Service init functions must return \(\*Foo, error\)`},
		},
		{
			name: "error_init_no_pointer",
			def: `
//encore:service
type Foo struct {}
func initFoo() (Foo, error) {}
`,
			wantErrs: []string{`.*Service init functions must return \(\*Foo, error\)`},
		},
		{
			name: "error_init_shadow_error",
			def: `
//encore:service
type Foo struct {}
func initFoo() (*Foo, error) {}
type error int
`,
			wantErrs: []string{`.*Service init functions must return \(\*Foo, error\)`},
		},
		{
			name: "error_init_bad_params",
			def: `
//encore:service
type Foo struct {}
func initFoo(int) (*Foo, error) {}
`,
			wantErrs: []string{`.*Service init functions cannot have parameters`},
		},
		{
			name: "error_invalid_grpc_service",
			def: `
//encore:service grpc=/foo
type Foo struct {}
`,
			wantErrs: []string{`.*The grpc field must be a valid.*(got "/foo")`},
		},
		{
			name: "error_missing_proto_file",
			def: `
//encore:service grpc=foo.Bar
type Foo struct {}
`,
			wantErrs: []string{`.*Unable to parse the protobuf file 'foo.proto'`},
		},
		{
			name: "error_missing_grpc_service",
			def: `
//encore:service grpc=foo.Bar
type Foo struct {}
-- proto/foo.proto --
syntax = "proto3";
package foo;
`,
			wantErrs: []string{`.*The gRPC service "Bar" cannot be found in the file "foo.proto"`},
		},
	}

	// testArchive renders the txtar archive to use for a given test.
	testArchive := func(test testCase) *txtar.Archive {
		importList := append([]string{"context"}, test.imports...)
		imports := ""
		if len(importList) > 0 {
			imports = "import (\n"
			for _, imp := range importList {
				imports += "\t" + strconv.Quote(imp) + "\n"
			}
			imports += ")\n"
		}

		return testutil.ParseTxtar(`
-- go.mod --
module example.com
require encore.dev v1.13.4
-- code.go --
package foo
` + imports + `

` + test.def + `
`)
	}

	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			c := qt.New(t)
			a := testArchive(test)
			tc := testutil.NewContext(c, false, a)
			tc.GoModDownload()

			l := pkginfo.New(tc.Context)
			schemaParser := schema.NewParser(tc.Context, l)
			protoParser := protoparse.NewParser(tc.Errs, []paths.FS{
				tc.MainModuleDir.Join("proto"),
			})

			if len(test.wantErrs) > 0 {
				defer tc.DeferExpectError(test.wantErrs...)
			} else {
				tc.FailTestOnErrors()
				defer tc.FailTestOnBailout()
			}

			pkg := l.MustLoadPkg(token.NoPos, "example.com")
			f := pkg.Files[0]
			gd := testutil.FindNodes[*ast.GenDecl](f.AST())[1]

			// Parse the directive from the func declaration.
			dir, doc, ok := directive.Parse(tc.Errs, gd.Doc)
			c.Assert(ok, qt.IsTrue)

			pd := ParseData{
				Errs:   tc.Errs,
				Loader: l,
				Proto:  protoParser,
				Schema: schemaParser,
				File:   f,
				Decl:   gd,
				Dir:    dir,
				Doc:    doc,
			}

			got := Parse(tc.Ctx, pd)
			if len(test.wantErrs) == 0 {
				// Check for equality, ignoring all the AST nodes and pkginfo types.
				cmpEqual := qt.CmpEquals(
					cmpopts.IgnoreInterfaces(struct{ ast.Node }{}),
					cmpopts.IgnoreTypes(&schema.FuncDecl{}, &schema.TypeDecl{}, &pkginfo.File{}, &pkginfo.Package{}, token.Pos(0)),
					cmpopts.EquateEmpty(),
					cmpopts.IgnoreUnexported(schema.StructField{}, schema.NamedType{}),
					cmp.Comparer(func(a, b *pkginfo.Package) bool {
						return a.ImportPath == b.ImportPath
					}),
				)
				c.Assert(got, cmpEqual, test.want)
			}
		})
	}
}

func fileForPkg(pkgName string, pkgPath paths.Pkg) *pkginfo.File {
	return &pkginfo.File{Pkg: &pkginfo.Package{
		Name:       pkgName,
		ImportPath: pkgPath,
	}}
}
