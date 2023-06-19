package grpcservice

import (
	"context"
	"path/filepath"
	"strings"

	"google.golang.org/protobuf/reflect/protoreflect"
	"google.golang.org/protobuf/types/descriptorpb"

	"encr.dev/pkg/paths"
	"encr.dev/v2/internals/perr"
	"encr.dev/v2/internals/pkginfo"
	"encr.dev/v2/internals/protoparse"
	"encr.dev/v2/parser/apis/internal/directive"
)

type Desc struct {
	// Service is the protobuf service descriptor.
	Service protoreflect.ServiceDescriptor
	// GenPkg is the Go package with the generated code.
	GenPkg *pkginfo.Package
}

// ParseGRPCDirective parses and validates the gRPC path directive.
func ParseGRPCDirective(ctx context.Context, errs *perr.List, loader *pkginfo.Loader, proto *protoparse.Parser, f directive.Field) (desc *Desc, ok bool) {
	astNode := f // directive.Field implements ast.Node
	grpcPath := f.Value
	// Two ways of referencing a service:
	// - "path/to/my.proto:ServiceName"
	// - "path.to.my.ServiceName"

	var (
		filePath string
		svcName  protoreflect.Name
	)
	if idx := strings.LastIndexByte(grpcPath, ':'); idx >= 0 {
		filePath = grpcPath[:idx]
		svcName = protoreflect.Name(grpcPath[idx+1:])
		if !svcName.IsValid() {
			errs.Add(errInvalidGRPCName(grpcPath).AtGoNode(astNode))
			return nil, false
		} else if !filepath.IsLocal(grpcPath) {
			errs.Add(errNonLocalGRPCPath(grpcPath).AtGoNode(astNode))
			return nil, false
		}
	} else {
		fullName := protoreflect.FullName(grpcPath)
		if !fullName.IsValid() {
			errs.Add(errInvalidGRPCName(grpcPath).AtGoNode(astNode))
			return nil, false
		}

		pkgpath := fullName.Parent()
		svcName = fullName.Name()
		if pkgpath == "" {
			// If there's no pkgpath we got a bare "Service" path, without a package name.
			errs.Add(errInvalidGRPCName(grpcPath).AtGoNode(astNode))
			return nil, false
		}
		filePath = strings.ReplaceAll(string(pkgpath), ".", "/") + ".proto"
	}

	file := proto.ParseFile(ctx, astNode, filePath)
	svc := file.Services().ByName(svcName)

	if svc == nil {
		errs.Add(errGRPCServiceNotFound(string(svcName), filePath).AtGoNode(astNode))
		return nil, false
	}

	desc = &Desc{Service: svc}

	options, ok := file.Options().(*descriptorpb.FileOptions)
	if ok && options != nil && options.GoPackage != nil && *options.GoPackage != "" {
		// We have an explicit go_package option. Use that.
		pkgPath, ok := paths.PkgPath(*options.GoPackage)
		if !ok {
			errs.Add(errInvalidGoPkg(grpcPath, *options.GoPackage).AtGoNode(astNode))
			return nil, false
		}
		desc.GenPkg = loader.MustLoadPkg(astNode.Pos(), pkgPath)
	} else {
		errs.Add(errNoGoPkgOption(grpcPath).AtGoNode(astNode))
		return nil, false
	}

	return desc, true
}
