package apis

import (
	"go/ast"
	"go/token"

	"encr.dev/v2/internal/parsectx"
	"encr.dev/v2/internal/pkginfo"
	"encr.dev/v2/internal/schema"
	"encr.dev/v2/parser/apis/api"
	"encr.dev/v2/parser/apis/authhandler"
	"encr.dev/v2/parser/apis/directive"
	"encr.dev/v2/parser/apis/middleware"
	"encr.dev/v2/parser/apis/servicestruct"
)

func NewParser(c *parsectx.Context, schema *schema.Parser) *Parser {
	return &Parser{
		c:      c,
		schema: schema,
	}
}

type Parser struct {
	c      *parsectx.Context
	schema *schema.Parser
}

// ParseResult describes the results of parsing a given package.
type ParseResult struct {
	RPCs           []*api.API
	AuthHandlers   []*authhandler.AuthHandler
	Middleware     []*middleware.Middleware
	ServiceStructs []*servicestruct.ServiceStruct
}

func (p *Parser) Parse(pkg *pkginfo.Package) ParseResult {
	var res ParseResult
	for _, file := range pkg.Files {
		for _, decl := range file.AST().Decls {
			switch decl := decl.(type) {
			case *ast.FuncDecl:
				if decl.Doc == nil {
					continue
				}

				dir, doc, err := directive.Parse(decl.Doc)
				if err != nil {
					p.c.Errs.Add(decl.Doc.Pos(), err.Error())
					continue
				} else if dir == nil {
					continue
				}

				switch dir.Name {
				case "api":
					r := api.Parse(api.ParseData{
						Errs:   p.c.Errs,
						Schema: p.schema,
						File:   file,
						Func:   decl,
						Dir:    dir,
						Doc:    doc,
					})
					res.RPCs = append(res.RPCs, r)

				case "authhandler":
					r := authhandler.Parse(authhandler.ParseData{
						Errs:   p.c.Errs,
						Schema: p.schema,
						File:   file,
						Func:   decl,
						Dir:    dir,
						Doc:    doc,
					})
					res.AuthHandlers = append(res.AuthHandlers, r)

				case "middleware":
					r := middleware.Parse(middleware.ParseData{
						Errs:   p.c.Errs,
						Schema: p.schema,
						File:   file,
						Func:   decl,
						Dir:    dir,
						Doc:    doc,
					})
					res.Middleware = append(res.Middleware, r)

				default:
					p.c.Errs.Addf(decl.Pos(), "unexpected directive %q on function declaration", dir.Name)
				}

			case *ast.GenDecl:
				if decl.Tok != token.TYPE {
					continue
				} else if decl.Doc == nil {
					continue
				}

				dir, doc, err := directive.Parse(decl.Doc)
				if err != nil {
					p.c.Errs.Add(decl.Doc.Pos(), err.Error())
					continue
				} else if dir == nil {
					continue
				}

				switch dir.Name {
				case "service":
					r := servicestruct.Parse(servicestruct.ParseData{
						Errs:   p.c.Errs,
						Schema: p.schema,
						File:   file,
						Decl:   decl,
						Dir:    dir,
						Doc:    doc,
					})
					res.ServiceStructs = append(res.ServiceStructs, r)

				default:
					p.c.Errs.Addf(decl.Pos(), "unexpected directive %q on function declaration", dir.Name)
				}
			}
		}
	}

	return res
}