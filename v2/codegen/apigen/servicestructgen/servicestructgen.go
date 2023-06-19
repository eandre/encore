package servicestructgen

import (
	"fmt"

	. "github.com/dave/jennifer/jen"

	"encr.dev/pkg/option"
	"encr.dev/v2/app"
	"encr.dev/v2/codegen"
	"encr.dev/v2/internals/schema"
	"encr.dev/v2/parser/apis/servicestruct"
)

func Gen(gen *codegen.Generator, svc *app.Service, s *servicestruct.ServiceStruct) *codegen.VarDecl {
	initFuncName := option.Map(s.Init, func(init *schema.FuncDecl) *Statement {
		return Id(init.Name)
	}).GetOrElse(Nil())

	f := gen.File(s.Decl.File.Pkg, "svcstruct")
	decl := f.VarDecl(s.Decl.Name).Value(Op("&").Qual("encore.dev/appruntime/apisdk/service", "Decl").Types(
		Id(s.Decl.Name),
	).Values(Dict{
		Id("Service"):     Lit(svc.Name),
		Id("Name"):        Lit(s.Decl.Name),
		Id("Setup"):       initFuncName,
		Id("SetupDefLoc"): Lit(gen.TraceNodes.SvcStruct(s)),
	}))

	f.Jen.Func().Id("init").Params().BlockFunc(func(g *Group) {
		g.Qual("encore.dev/appruntime/apisdk/service", "Register").Call(decl.Qual())

		if grpc, ok := s.GRPC.Get(); ok {
			pkgPath := grpc.GenPkg.ImportPath.String()
			g.Qual("encore.dev/appruntime/apisdk/api", "RegisterGRPCService").
				Types(Qual(pkgPath, fmt.Sprintf("%sServer", grpc.Service.Name()))).
				Call(
					decl.Qual(),
					Qual(pkgPath, fmt.Sprintf("Register%sServer", grpc.Service.Name())),
				)
		}
	})

	return decl
}
