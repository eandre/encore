"""
API endpoint definitions for Encore applications.

This module provides decorators and classes for defining HTTP API endpoints.
"""

from __future__ import annotations

from dataclasses import dataclass, field
from datetime import date, datetime
from typing import (
    Any,
    Callable,
    Generic,
    Literal,
    ParamSpec,
    Protocol,
    TypeVar,
    overload,
)

from encoredev.req_meta import Method


@dataclass
class APIOptions:
    """Options for API endpoint configuration."""

    method: Method | list[Method] | Literal["*"] = "POST"
    """
    The HTTP method(s) to match for this endpoint.
    Use "*" to match any method.
    """

    path: str | None = None
    """
    The request path to match for this endpoint.

    Use `:` to define single-segment parameters, e.g. `/users/:id`.
    Use `*` to match any number of segments, e.g. `/files/*path`.

    If not specified, it defaults to `/<service-name>.<endpoint-name>`.
    """

    expose: bool = False
    """
    Whether or not to make this endpoint publicly accessible.
    If False, the endpoint is only accessible from the internal network.
    """

    auth: bool = False
    """
    Whether or not the request must contain valid authentication credentials.
    If set to True and the request is not authenticated,
    Encore returns a 401 Unauthorized error.
    """

    body_limit: int | None = None
    """
    The maximum body size, in bytes. If the request body exceeds this value,
    Encore stops request processing and returns an error.

    If left unspecified it defaults to a reasonable default (currently 2MiB).
    If set to None, the body size is unlimited.
    """

    tags: list[str] = field(default_factory=list)
    """Tags to filter endpoints when generating clients and in middlewares."""

    sensitive: bool = False
    """
    When set to True, request information such as payloads and headers
    will be excluded from traces.
    """


@dataclass
class StreamOptions:
    """Options for streaming endpoint configuration."""

    path: str | None = None
    """
    The request path to match for this endpoint.

    Use `:` to define single-segment parameters, e.g. `/users/:id`.
    Use `*` to match any number of segments, e.g. `/files/*path`.

    If not specified, it defaults to `/<service-name>.<endpoint-name>`.
    """

    expose: bool = False
    """
    Whether or not to make this endpoint publicly accessible.
    If False, the endpoint is only accessible from the internal network.
    """

    auth: bool = False
    """
    Whether or not the request must contain valid authentication credentials.
    If set to True and the request is not authenticated,
    Encore returns a 401 Unauthorized error.
    """

    tags: list[str] = field(default_factory=list)
    """Tags to filter endpoints when generating clients and in middlewares."""

    sensitive: bool = False
    """
    When set to True, request information such as payloads and headers
    will be excluded from traces.
    """


@dataclass
class StaticOptions:
    """Options for static file serving endpoint configuration."""

    dir: str
    """
    The relative path to the directory containing the static files to serve.
    The provided path must be a subdirectory from the calling file's directory.
    """

    path: str | None = None
    """
    The request path to match for this endpoint.

    Use `:` to define single-segment parameters, e.g. `/users/:id`.
    Use `*` to match any number of segments, e.g. `/files/*path`.

    If not specified, it defaults to `/<service-name>.<endpoint-name>`.
    """

    expose: bool = False
    """
    Whether or not to make this endpoint publicly accessible.
    If False, the endpoint is only accessible from the internal network.
    """

    auth: bool = False
    """
    Whether or not the request must contain valid authentication credentials.
    If set to True and the request is not authenticated,
    Encore returns a 401 Unauthorized error.
    """

    not_found: str | None = None
    """
    Path to the file to serve when the requested file is not found.
    The path must be a relative path within the calling file's directory.
    """

    not_found_status: int = 404
    """HTTP Status code used when serving notFound fallback. Defaults to 404."""

    headers: dict[str, str | list[str]] | None = None
    """Custom HTTP headers to apply to all static files served."""


class StaticAssets:
    """Static assets endpoint definition."""

    def __init__(self, options: StaticOptions) -> None:
        self.options = options


# Type aliases for type annotations
Header = TypeVar("Header", str, int, bool, datetime)
"""Type marker for header parameters."""

Query = TypeVar("Query", str, list[str], int, list[int], bool, list[bool], datetime, list[datetime])
"""Type marker for query parameters."""


@dataclass
class CookieWithOptions(Generic[TypeVar("T")]):
    """Cookie with additional options."""

    value: Any
    expires: datetime | None = None
    same_site: Literal["Strict", "Lax", "None"] | None = None
    domain: str | None = None
    path: str | None = None
    max_age: int | None = None
    secure: bool | None = None
    http_only: bool | None = None
    partitioned: bool | None = None


Cookie = CookieWithOptions
"""Type marker for cookie parameters."""


P = ParamSpec("P")
R = TypeVar("R")


class APIDecorator(Protocol):
    """Protocol for API endpoint decorators."""

    @overload
    def __call__(self, options: APIOptions) -> Callable[[Callable[P, R]], Callable[P, R]]: ...

    @overload
    def __call__(
        self, options: APIOptions, fn: Callable[P, R]
    ) -> Callable[P, R]: ...


def api(
    options: APIOptions | None = None,
    *,
    method: Method | list[Method] | Literal["*"] = "POST",
    path: str | None = None,
    expose: bool = False,
    auth: bool = False,
    body_limit: int | None = None,
    tags: list[str] | None = None,
    sensitive: bool = False,
) -> Callable[[Callable[P, R]], Callable[P, R]]:
    """
    Decorator for defining API endpoints.

    Can be used either with an APIOptions object or with keyword arguments.

    Example:
        @api(method="GET", path="/users/:id")
        async def get_user(id: int) -> User:
            ...

        @api(APIOptions(method="POST", path="/users"))
        async def create_user(data: CreateUserRequest) -> User:
            ...
    """
    if options is None:
        options = APIOptions(
            method=method,
            path=path,
            expose=expose,
            auth=auth,
            body_limit=body_limit,
            tags=tags or [],
            sensitive=sensitive,
        )

    def decorator(fn: Callable[P, R]) -> Callable[P, R]:
        # Store the options on the function for later introspection
        fn.__encore_api__ = options  # type: ignore
        return fn

    return decorator


# Attach additional methods to the api function
def _api_raw(
    options: APIOptions | None = None,
    *,
    method: Method | list[Method] | Literal["*"] = "POST",
    path: str | None = None,
    expose: bool = False,
    auth: bool = False,
    body_limit: int | None = None,
    tags: list[str] | None = None,
    sensitive: bool = False,
) -> Callable[[Callable[P, R]], Callable[P, R]]:
    """
    Decorator for defining raw API endpoints.

    Raw endpoints receive the raw HTTP request and response objects.
    """
    if options is None:
        options = APIOptions(
            method=method,
            path=path,
            expose=expose,
            auth=auth,
            body_limit=body_limit,
            tags=tags or [],
            sensitive=sensitive,
        )

    def decorator(fn: Callable[P, R]) -> Callable[P, R]:
        fn.__encore_api__ = options  # type: ignore
        fn.__encore_raw__ = True  # type: ignore
        return fn

    return decorator


def _api_static(options: StaticOptions) -> StaticAssets:
    """Create a static assets endpoint."""
    return StaticAssets(options)


def _api_stream_in_out(
    options: StreamOptions | None = None,
    *,
    path: str | None = None,
    expose: bool = False,
    auth: bool = False,
    tags: list[str] | None = None,
    sensitive: bool = False,
) -> Callable[[Callable[P, R]], Callable[P, R]]:
    """Decorator for defining bidirectional streaming endpoints."""
    if options is None:
        options = StreamOptions(
            path=path,
            expose=expose,
            auth=auth,
            tags=tags or [],
            sensitive=sensitive,
        )

    def decorator(fn: Callable[P, R]) -> Callable[P, R]:
        fn.__encore_stream__ = options  # type: ignore
        fn.__encore_stream_type__ = "inout"  # type: ignore
        return fn

    return decorator


def _api_stream_in(
    options: StreamOptions | None = None,
    *,
    path: str | None = None,
    expose: bool = False,
    auth: bool = False,
    tags: list[str] | None = None,
    sensitive: bool = False,
) -> Callable[[Callable[P, R]], Callable[P, R]]:
    """Decorator for defining input streaming endpoints."""
    if options is None:
        options = StreamOptions(
            path=path,
            expose=expose,
            auth=auth,
            tags=tags or [],
            sensitive=sensitive,
        )

    def decorator(fn: Callable[P, R]) -> Callable[P, R]:
        fn.__encore_stream__ = options  # type: ignore
        fn.__encore_stream_type__ = "in"  # type: ignore
        return fn

    return decorator


def _api_stream_out(
    options: StreamOptions | None = None,
    *,
    path: str | None = None,
    expose: bool = False,
    auth: bool = False,
    tags: list[str] | None = None,
    sensitive: bool = False,
) -> Callable[[Callable[P, R]], Callable[P, R]]:
    """Decorator for defining output streaming endpoints."""
    if options is None:
        options = StreamOptions(
            path=path,
            expose=expose,
            auth=auth,
            tags=tags or [],
            sensitive=sensitive,
        )

    def decorator(fn: Callable[P, R]) -> Callable[P, R]:
        fn.__encore_stream__ = options  # type: ignore
        fn.__encore_stream_type__ = "out"  # type: ignore
        return fn

    return decorator


# Attach additional functions to the api object
api.raw = _api_raw  # type: ignore
api.static = _api_static  # type: ignore
api.stream_in_out = _api_stream_in_out  # type: ignore
api.stream_in = _api_stream_in  # type: ignore
api.stream_out = _api_stream_out  # type: ignore


__all__ = [
    "api",
    "APIOptions",
    "Cookie",
    "CookieWithOptions",
    "Header",
    "Query",
    "StaticAssets",
    "StaticOptions",
    "StreamOptions",
]
