package api

import (
	"context"
	"reflect"
	"sync"

	"google.golang.org/grpc"
	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/reflection"
	"google.golang.org/grpc/status"
	"google.golang.org/protobuf/encoding/protojson"
	"google.golang.org/protobuf/proto"

	"encore.dev/appruntime/exported/model"
	"encore.dev/internal/platformauth"
)

// GRPCDesc describes a gRPC endpoint.
type GRPCDesc[Req, Resp any] struct {
	// SvcNum is the 1-based index into the list of services.
	SvcNum uint16

	// Service and Endpoint name the API this description is for.
	Service  string
	Endpoint string

	Path   string
	DefLoc uint32

	rpcDescOnce   sync.Once
	cachedRPCDesc *model.RPCDesc
}

type GRPCHandler interface {
	// FullMethod returns the gRPC full method path ("/path.to.package.ServiceName/MethodName")
	FullMethod() string
	// HandleUnary handles gRPC unary requests.
	HandleUnary(ctx context.Context, req any, info *grpc.UnaryServerInfo, handler grpc.UnaryHandler) (resp any, err error) {
}

type grpcRegistrar func(srv *grpc.Server) error

type grpcServiceDesc struct {
	svcName   string
	registrar grpcRegistrar
	handlers  []GRPCHandler
}

func (s *Server) registerGRPCService(desc grpcServiceDesc) {
	s.grpcServices = append(s.grpcServices, desc)
	for _, h := range desc.handlers {
		meth := h.FullMethod()
		if s.grpcHandlers[meth] != nil {
			panic("internal error: duplicate gRPC handler registered for " + meth)
		}
		s.grpcHandlers[meth] = h
	}
}

func (s *Server) InitializeGRPC() error {
	if len(s.grpcServices) == 0 {
		return nil
	}

	s.grpcSrv = grpc.NewServer(
		grpc.UnaryInterceptor(
			func(ctx context.Context, req any, info *grpc.UnaryServerInfo, handler grpc.UnaryHandler) (resp any, err error) {
				ep, ok := s.grpcHandlers[info.FullMethod]
				if !ok {
					return nil, status.Errorf(codes.Unimplemented, "method %q not implemented", info.FullMethod)
				}

			},
		),
	)
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

func (d *GRPCDesc[Req, Resp]) HandleUnary(ctx context.Context, req any, info *grpc.UnaryServerInfo, handler grpc.UnaryHandler) (resp any, err error) {
	s.beginOperation()
	defer s.finishOperation()

	// Serialize the payload for tracing.
	var nonRawPayload []byte
	if msg, ok := req.(proto.Message); ok {
		data, err := (protojson.MarshalOptions{
			Multiline:     true,
			Indent:        "  ",
			AllowPartial:  true,
			UseProtoNames: false,
		}).Marshal(msg)
		if err == nil {
			nonRawPayload = data
		}
	}

	_, err = s.beginRequest(ctx, &beginRequestParams{
		Type: model.RPCCall,
		//DefLoc:        d.DefLoc,
		//TraceID:       c.traceID,
		//ParentSpanID:  c.parentSpanID,
		//CallerEventID: c.parentEventID,

		Data: &model.RPCData{
			Desc: &model.RPCDesc{
				Service:      "",
				SvcNum:       0,
				Endpoint:     "",
				AuthHandler:  false,
				Raw:          false,
				RequestType:  nil,
				ResponseType: nil,
			},
			HTTPMethod:    "POST", // gRPC is always POST effectively
			Path:          info.FullMethod,
			PathParams:    nil,
			TypedPayload:  req,
			NonRawPayload: nonRawPayload,
			//UserID:               c.auth.UID,
			//AuthData:             c.auth.UserData,
			//RequestHeaders:       c.req.Header,
			FromEncorePlatform: platformauth.IsEncorePlatformRequest(ctx),
			//ServiceToServiceCall: c.isEncoreToEncoreCall,
		},

		//ExtRequestID:     clampTo64Chars(c.req.Header.Get("X-Request-ID")),
		//ExtCorrelationID: clampTo64Chars(c.req.Header.Get("X-Correlation-ID")),
	})
	if err != nil {
		return nil, err
	}
	resp, err = handler(ctx, req)

	if err != nil {
		// TODO handle gRPC error conversion
		s.finishRequest(newErrResp(err, 0))
	} else {
		modelResp := newResp(resp, 0, nil, false, nil, nil, s.json)
		s.finishRequest(modelResp)
	}

	return resp, err
}

// rpcDesc returns the RPC description for this endpoint,
// computing and caching the first time it's called.
func (d *GRPCDesc[Req, Resp]) rpcDesc() *model.RPCDesc {
	d.rpcDescOnce.Do(func() {
		// TODO handle google.protobuf.Empty as void?
		var reqTyp Req
		desc := &model.RPCDesc{
			Service:     d.Service,
			SvcNum:      d.SvcNum,
			Endpoint:    d.Endpoint,
			Raw:         false,
			RequestType: reflect.TypeOf(reqTyp),
		}

		if !isVoid[Resp]() {
			var typ Resp
			desc.ResponseType = reflect.TypeOf(typ)
		}
		d.cachedRPCDesc = desc
	})
	return d.cachedRPCDesc
}
