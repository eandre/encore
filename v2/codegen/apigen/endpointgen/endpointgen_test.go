package endpointgen

import (
	"testing"

	"encr.dev/v2/app"
	"encr.dev/v2/codegen"
	"encr.dev/v2/codegen/internal/codegentest"
)

func TestCodegen(t *testing.T) {
	fn := func(gen *codegen.Generator, desc *app.Desc) {
		Gen(gen, desc.Services[0])
	}
	codegentest.Run(t, fn)
}