package builderimpl

import (
	"encore.dev/appruntime/exported/experiments"
	"encr.dev/pkg/appfile"
	"encr.dev/pkg/builder"
	"encr.dev/v2/pybuilder"
	"encr.dev/v2/tsbuilder"
	"encr.dev/v2/v2builder"
)

func Resolve(lang appfile.Lang, expSet *experiments.Set) builder.Impl {
	switch lang {
	case appfile.LangPy:
		return pybuilder.New()
	case appfile.LangTS:
		return tsbuilder.New()
	default:
		return v2builder.New()
	}
}
