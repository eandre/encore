"""
Middleware support for Encore applications.

This module provides functionality for defining and using middleware
in Encore API endpoints.
"""

from __future__ import annotations

from dataclasses import dataclass, field
from typing import Any, Awaitable, Callable, Protocol

from encoredev import current_request
from encoredev.req_meta import RequestMeta


@dataclass
class MiddlewareOptions:
    """Configuration for middleware targeting."""

    target: "MiddlewareTarget | None" = None
    """Configuration for what endpoints should be targeted by the middleware."""


@dataclass
class MiddlewareTarget:
    """Configuration for what endpoints that should be targeted by the middleware."""

    expose: bool | None = None
    """If set, only run middleware on endpoints that are either exposed or not exposed."""

    auth: bool | None = None
    """If set, only run middleware on endpoints that either require or not require auth."""

    is_raw: bool | None = None
    """If set, only run middleware on endpoints that are raw endpoints."""

    is_stream: bool | None = None
    """If set, only run middleware on endpoints that are stream endpoints."""

    tags: list[str] = field(default_factory=list)
    """
    If set, only run middleware on endpoints that have specific tags.
    These tags are evaluated with OR, meaning the middleware applies to an
    API if the API has at least one of those tags.
    """


class MiddlewareRequest:
    """
    Request object passed to middleware functions.

    Provides access to request metadata, raw request/response objects,
    and a data dictionary for passing data between middlewares.
    """

    def __init__(
        self,
        stream: Any | None = None,
        raw_request: Any | None = None,
        raw_response: Any | None = None,
    ) -> None:
        self._req_meta: RequestMeta | None = None
        self._stream = stream
        self._raw_request = raw_request
        self._raw_response = raw_response
        self._data: dict[str, Any] | None = None

    @property
    def request_meta(self) -> RequestMeta | None:
        """
        Request metadata for typed and stream handlers.
        For raw handlers, see raw_request and raw_response.
        """
        if self._req_meta is None:
            self._req_meta = current_request()
        return self._req_meta

    @property
    def stream(self) -> Any | None:
        """Stream object for stream handlers."""
        return self._stream

    @property
    def raw_request(self) -> Any | None:
        """Raw request object for raw request handlers."""
        return self._raw_request

    @property
    def raw_response(self) -> Any | None:
        """Raw response object for raw request handlers."""
        return self._raw_response

    @property
    def data(self) -> dict[str, Any]:
        """
        Dictionary for passing data from middlewares to the handler.
        The data will be available via `current_request()`.
        """
        if self._data is None:
            self._data = {}
        return self._data


class ResponseHeader:
    """Response header builder for middleware."""

    def __init__(self) -> None:
        self.headers: dict[str, str | list[str]] = {}

    def set(self, key: str, value: str | list[str]) -> None:
        """
        Set a header value for a key.
        If a previous middleware has already set a value, it will be overridden.
        """
        self.headers[key] = value

    def add(self, key: str, value: str | list[str]) -> None:
        """
        Add a header value to a key.
        If a previous middleware has already set a value, they will be appended.
        """
        prev = self.headers.get(key)

        if prev is None:
            self.headers[key] = value
        else:
            if isinstance(prev, list):
                if isinstance(value, list):
                    self.headers[key] = prev + value
                else:
                    self.headers[key] = prev + [value]
            else:
                if isinstance(value, list):
                    self.headers[key] = [prev] + value
                else:
                    self.headers[key] = [prev, value]


class HandlerResponse:
    """
    Response object returned from handlers and middleware.

    Allows middleware to modify the response payload, headers, and status code.
    """

    def __init__(self, payload: Any = None) -> None:
        self.payload = payload
        """The payload returned by the handler."""

        self._headers: ResponseHeader | None = None
        self._status: int | None = None

    @property
    def header(self) -> ResponseHeader:
        """
        Header builder for setting response headers.
        Only works for typed handlers. For raw handlers see MiddlewareRequest.raw_response.
        """
        if self._headers is None:
            self._headers = ResponseHeader()
        return self._headers

    @property
    def status(self) -> int | None:
        """Get the HTTP status code override."""
        return self._status

    @status.setter
    def status(self, value: int) -> None:
        """Override the HTTP status code for successful requests."""
        self._status = value


# Type alias for the next function in middleware chain
Next = Callable[[MiddlewareRequest], Awaitable[HandlerResponse]]

# Type alias for middleware function
MiddlewareFn = Callable[[MiddlewareRequest, Next], Awaitable[HandlerResponse]]


class Middleware(Protocol):
    """Protocol for middleware with optional options."""

    options: MiddlewareOptions | None

    def __call__(
        self, req: MiddlewareRequest, next: Next
    ) -> Awaitable[HandlerResponse]: ...


def middleware(
    fn_or_options: MiddlewareFn | MiddlewareOptions,
    fn: MiddlewareFn | None = None,
) -> Middleware:
    """
    Create a middleware function.

    Can be used in two ways:

    1. With just a function:
        @middleware
        async def my_middleware(req, next):
            response = await next(req)
            return response

    2. With options and a function:
        @middleware(MiddlewareOptions(target=MiddlewareTarget(auth=True)))
        async def auth_middleware(req, next):
            response = await next(req)
            return response
    """
    if fn is None:
        # Called as @middleware without options
        if callable(fn_or_options):
            mw_fn = fn_or_options
            mw_fn.options = None  # type: ignore
            return mw_fn  # type: ignore
        else:
            # Called as @middleware(options) - return decorator
            options = fn_or_options

            def decorator(f: MiddlewareFn) -> Middleware:
                def wrapper(
                    req: MiddlewareRequest, next: Next
                ) -> Awaitable[HandlerResponse]:
                    return f(req, next)

                wrapper.options = options  # type: ignore
                return wrapper  # type: ignore

            return decorator  # type: ignore
    else:
        # Called as middleware(options, fn)
        options = fn_or_options

        def wrapper(req: MiddlewareRequest, next: Next) -> Awaitable[HandlerResponse]:
            return fn(req, next)

        wrapper.options = options  # type: ignore
        return wrapper  # type: ignore


__all__ = [
    "HandlerResponse",
    "Middleware",
    "MiddlewareFn",
    "MiddlewareOptions",
    "MiddlewareRequest",
    "MiddlewareTarget",
    "middleware",
    "Next",
    "ResponseHeader",
]
