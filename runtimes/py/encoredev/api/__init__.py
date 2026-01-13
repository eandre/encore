"""
API module for Encore applications.

This module provides functionality for defining HTTP API endpoints,
handling errors, middleware, and streaming.
"""

from encoredev.api.error import APIError, ErrCode
from encoredev.api.httpstatus import HttpStatus
from encoredev.api.endpoint import (
    api,
    APIOptions,
    StreamOptions,
    StaticOptions,
    StaticAssets,
    Header,
    Query,
    Cookie,
)
from encoredev.api.middleware import (
    middleware,
    Middleware,
    MiddlewareOptions,
    MiddlewareRequest,
    HandlerResponse,
    ResponseHeader,
)
from encoredev.api.gateway import Gateway, GatewayConfig
from encoredev.api.stream import StreamIn, StreamOut, StreamInOut

__all__ = [
    # Endpoint
    "api",
    "APIOptions",
    "StreamOptions",
    "StaticOptions",
    "StaticAssets",
    "Header",
    "Query",
    "Cookie",
    # Error handling
    "APIError",
    "ErrCode",
    # HTTP status
    "HttpStatus",
    # Middleware
    "middleware",
    "Middleware",
    "MiddlewareOptions",
    "MiddlewareRequest",
    "HandlerResponse",
    "ResponseHeader",
    # Gateway
    "Gateway",
    "GatewayConfig",
    # Streaming
    "StreamIn",
    "StreamOut",
    "StreamInOut",
]
