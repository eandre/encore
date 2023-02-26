package api

import (
	"go/ast"
	"go/token"
	"strconv"
	"testing"

	qt "github.com/frankban/quicktest"
	"github.com/google/go-cmp/cmp"
	"github.com/google/go-cmp/cmp/cmpopts"
	"github.com/rogpeppe/go-internal/txtar"

	"encr.dev/v2/internal/pkginfo"
	schema2 "encr.dev/v2/internal/schema"
	"encr.dev/v2/internal/testutil"
	"encr.dev/v2/parser/apis/api/apipaths"
	"encr.dev/v2/parser/apis/directive"
	"encr.dev/v2/parser/apis/selector"
)

func TestParseRPC(t *testing.T) {
	type testCase struct {
		name     string
		imports  []string
		def      string
		want     *API
		wantErrs []string
	}
	tests := []testCase{
		{
			name: "basic",
			def: `
// Foo does things.
//encore:api public
func Foo(ctx context.Context) error {}
`,
			want: &API{
				Name:   "Foo",
				Doc:    "Foo does things.\n",
				Access: Public,
				Path: apipaths.Path{Segments: []apipaths.Segment{
					{Type: apipaths.Literal, Value: "foo.Foo", ValueType: schema2.String},
				}},
				HTTPMethods: []string{"GET", "POST"},
			},
		},
		{
			name: "with_fields",
			def: `
//encore:api private path=/foo method=PUT tag:some-tag
func Foo(ctx context.Context) error {}
`,
			want: &API{
				Name:   "Foo",
				Doc:    "",
				Access: Private,
				Path: apipaths.Path{Segments: []apipaths.Segment{
					{Type: apipaths.Literal, Value: "foo", ValueType: schema2.String},
				}},
				HTTPMethods: []string{"PUT"},
				Tags:        selector.Set{{Type: selector.Tag, Value: "some-tag"}},
			},
		},
		{
			name: "with_string_param",
			def: `
//encore:api auth path=/:key
func Foo(ctx context.Context, key string) error {}
`,
			want: &API{
				Name:   "Foo",
				Doc:    "",
				Access: Auth,
				Path: apipaths.Path{Segments: []apipaths.Segment{
					{Type: apipaths.Param, Value: "key", ValueType: schema2.String},
				}},
				HTTPMethods: []string{"GET", "POST"},
			},
		},
		{
			name: "with_int_param",
			def: `
//encore:api auth path=/:key
func Foo(ctx context.Context, key int) error {}
`,
			want: &API{
				Name:   "Foo",
				Doc:    "",
				Access: Auth,
				Path: apipaths.Path{Segments: []apipaths.Segment{
					{Type: apipaths.Param, Value: "key", ValueType: schema2.Int},
				}},
				HTTPMethods: []string{"GET", "POST"},
			},
		},
		{
			name:    "raw",
			imports: []string{"net/http"},
			def: `
//encore:api public raw path=/raw
func Raw(w http.ResponseWriter, req *http.Request) {}
`,
			want: &API{
				Name:   "Raw",
				Doc:    "",
				Access: Public,
				Raw:    true,
				Path: apipaths.Path{Segments: []apipaths.Segment{
					{Type: apipaths.Literal, Value: "raw", ValueType: schema2.String},
				}},
				HTTPMethods: []string{"*"},
			},
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
			schemaParser := schema2.NewParser(tc.Context, l)

			if len(test.wantErrs) > 0 {
				defer tc.DeferExpectError(test.wantErrs...)
			} else {
				tc.FailTestOnErrors()
				defer tc.FailTestOnBailout()
			}

			pkg := l.MustLoadPkg(token.NoPos, "example.com")
			f := pkg.Files[0]
			fd := testutil.FindNodes[*ast.FuncDecl](f.AST())[0]

			// Parse the directive from the func declaration.
			dir, doc, err := directive.Parse(fd.Doc)
			c.Assert(err, qt.IsNil)
			pd := ParseData{
				Errs:   tc.Errs,
				Schema: schemaParser,
				File:   f,
				Func:   fd,
				Dir:    dir,
				Doc:    doc,
			}

			got := Parse(pd)
			if len(test.wantErrs) == 0 {
				// Check for equality, ignoring all the AST nodes and pkginfo types.
				cmpEqual := qt.CmpEquals(
					cmpopts.IgnoreInterfaces(struct{ ast.Node }{}),
					cmpopts.IgnoreTypes(&schema2.FuncDecl{}, &schema2.TypeDecl{}, &pkginfo.File{}, &pkginfo.Package{}, token.Pos(0)),
					cmpopts.EquateEmpty(),
					cmpopts.IgnoreUnexported(schema2.StructField{}, schema2.NamedType{}),
					cmp.Comparer(func(a, b *pkginfo.Package) bool {
						return a.ImportPath == b.ImportPath
					}),
				)
				c.Assert(got, cmpEqual, test.want)
			}
		})
	}
}