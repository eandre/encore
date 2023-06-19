//go:build encore_app

package api

import (
	"fmt"
	"reflect"
	"runtime/debug"

	"google.golang.org/grpc"

	"encore.dev/appruntime/apisdk/service"
)

func RegisterEndpoint(handler Handler) {
	Singleton.registerEndpoint(handler)
}

func RegisterAuthHandler(handler AuthHandler) {
	Singleton.setAuthHandler(handler)
}

// RegisterAuthDataType registers the type of the auth data that will be
// returned by the auth handler. This is used to verify that the auth data
// returned by the auth handler is of the correct type.
//
// Note type T is required to be a pointer type.
func RegisterAuthDataType[T any]() {
	var zero T
	RegisteredAuthDataType = reflect.TypeOf(zero)
}

func RegisterGlobalMiddleware(mw *Middleware) {
	Singleton.registerGlobalMiddleware(mw)
}

// RegisterGRPCService registers a new gRPC service to be served.
// It's called by generated code.
func RegisterGRPCService[T any](init service.Initializer, registerFunc func(grpc.ServiceRegistrar, T)) {
	fn := func(srv *grpc.Server) (err error) {
		defer func() {
			if e := recover(); e != nil {
				err = fmt.Errorf("service %s panicked while initializing: %v\n%s",
					init.ServiceName(), e, debug.Stack())
			}
		}()

		ss, err := init.GetDecl()
		if err != nil {
			return err
		}

		t, ok := ss.(T)
		if !ok {
			return fmt.Errorf("internal error: service %s does not implement gRPC service interface",
				init.ServiceName())
		}
		registerFunc(srv, t)
		return nil
	}

	Singleton.registerGRPCService(grpcServiceDesc{
		svcName:   init.ServiceName(),
		registrar: fn,
	})
}
