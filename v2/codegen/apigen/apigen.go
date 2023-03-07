package apigen

import (
	"golang.org/x/exp/maps"

	"encr.dev/pkg/option"
	"encr.dev/v2/app"
	"encr.dev/v2/codegen"
	"encr.dev/v2/codegen/apigen/authhandlergen"
	"encr.dev/v2/codegen/apigen/endpointgen"
	"encr.dev/v2/codegen/apigen/maingen"
	"encr.dev/v2/codegen/apigen/middlewaregen"
	"encr.dev/v2/internal/pkginfo"
	"encr.dev/v2/parser/apis/api"
	"encr.dev/v2/parser/apis/middleware"
)

func Process(gg *codegen.Generator, desc *app.Desc, mainModule *pkginfo.Module) {
	gp := maingen.GenParams{
		Gen:         gg,
		Desc:        desc,
		MainModule:  mainModule,
		APIHandlers: make(map[*api.Endpoint]*codegen.VarDecl),
		Middleware:  make(map[*middleware.Middleware]*codegen.VarDecl),
		AuthHandler: option.None[*codegen.VarDecl](),
	}

	fw := desc.Framework.MustGet()
	for _, svc := range desc.Services {
		eps := endpointgen.Gen(gg, svc)
		maps.Copy(gp.APIHandlers, eps)

		if svc.Framework.IsPresent() {
			mws := middlewaregen.Gen(gg, svc.Framework.MustGet().Middleware)
			maps.Copy(gp.Middleware, mws)
		}
	}

	mws := middlewaregen.Gen(gg, fw.GlobalMiddleware)
	maps.Copy(gp.Middleware, mws)

	if fw.AuthHandler.IsPresent() {
		gp.AuthHandler = option.Some(authhandlergen.Gen(gg, fw.AuthHandler.MustGet()))
	}

	maingen.Gen(gp)
}
