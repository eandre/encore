package api

import (
	"google.golang.org/grpc"
	"google.golang.org/grpc/reflection"
)

type grpcRegistrar func(srv *grpc.Server) error

type grpcServiceDesc struct {
	svcName   string
	registrar grpcRegistrar
}

func (s *Server) registerGRPCService(desc grpcServiceDesc) {
	s.grpcServices = append(s.grpcServices, desc)
}

func (s *Server) InitializeGRPC() error {
	if len(s.grpcServices) == 0 {
		return nil
	}
	s.grpcSrv = grpc.NewServer()
	for _, desc := range s.grpcServices {
		if err := desc.registrar(s.grpcSrv); err != nil {
			return err
		}
	}
	pluralS := ""
	if len(s.grpcServices) > 1 {
		pluralS = "s"
	}
	s.rootLogger.Info().Msgf("registered %d gRPC service%s",
		len(s.grpcServices), pluralS)

	if s.runtime.EnvType != "production" {
		reflection.Register(s.grpcSrv)
	}

	return nil
}
