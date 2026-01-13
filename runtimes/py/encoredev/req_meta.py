"""
Request metadata for Encore applications.

This module provides access to metadata about the current request being processed,
including API call details and Pub/Sub message information.
"""

from __future__ import annotations

from dataclasses import dataclass
from typing import Any, Literal, Union

from encoredev.internal.runtime import get_current_request


Method = Literal[
    "GET", "POST", "PUT", "PATCH", "DELETE", "HEAD", "OPTIONS", "CONNECT", "TRACE"
]
"""HTTP method type."""


@dataclass(frozen=True)
class APIDesc:
    """Describes an API endpoint."""

    service: str
    """The name of the service that the endpoint belongs to."""

    endpoint: str
    """The name of the endpoint itself."""

    raw: bool
    """Whether the endpoint is a raw endpoint."""

    auth: bool
    """Whether the endpoint requires auth."""

    tags: list[str]
    """Tags specified on the endpoint."""


@dataclass(frozen=True)
class TraceData:
    """Provides information about the active trace."""

    trace_id: str
    """The trace id."""

    span_id: str
    """The current span id."""

    parent_trace_id: str | None = None
    """The trace id that initiated this trace, if any."""

    parent_span_id: str | None = None
    """The span that initiated this span, if any."""

    ext_correlation_id: str | None = None
    """
    The external correlation id provided when the trace was created, if any.
    For example via the `Request-Id` or `X-Correlation-Id` headers.
    """


@dataclass(frozen=True)
class APICallMeta:
    """Describes an API call being processed."""

    type: Literal["api-call"]
    """Specifies that the request is an API call."""

    api: APIDesc
    """Describes the API Endpoint being called."""

    method: Method
    """The HTTP method used in the API call."""

    path: str
    """
    The request URL path used in the API call, excluding any query string parameters.
    For example "/path/to/endpoint".
    """

    path_and_query: str
    """
    The request URL path used in the API call, including any query string parameters.
    For example "/path/to/endpoint?with=querystring".
    """

    path_params: dict[str, Any]
    """
    The parsed path parameters for the API endpoint.
    The keys are the names of the path parameters, from the API definition.
    For example {"id": 5}.
    """

    headers: dict[str, str | list[str]]
    """
    The request headers from the HTTP request.
    The values are lists if the header contains multiple values,
    either separated by ";" or when the header key appears more than once.
    """

    parsed_payload: dict[str, Any] | None = None
    """
    The parsed request payload, as expected by the application code.
    Not provided for raw endpoints or when the API endpoint expects no request data.
    """

    middleware_data: dict[str, Any] | None = None
    """Contains values set in middlewares via `MiddlewareRequest.data`."""

    trace: TraceData | None = None
    """Information about the trace, if the request is being traced."""


@dataclass(frozen=True)
class PubSubMessageMeta:
    """Describes a Pub/Sub message being processed."""

    type: Literal["pubsub-message"]
    """Specifies that the request is a Pub/Sub message."""

    service: str
    """The service processing the message."""

    topic: str
    """The name of the Pub/Sub topic."""

    subscription: str
    """The name of the Pub/Sub subscription."""

    message_id: str
    """
    The unique id of the Pub/Sub message.
    It is the same id returned by `topic.publish()`.
    The message id stays the same across delivery attempts.
    """

    delivery_attempt: int
    """The delivery attempt. The first attempt starts at 1, and increases by 1 for each retry."""

    parsed_payload: dict[str, Any] | None = None
    """The parsed request payload, as expected by the application code."""

    trace: TraceData | None = None
    """Information about the trace, if the request is being traced."""


RequestMeta = Union[APICallMeta, PubSubMessageMeta]
"""Describes an API call or Pub/Sub message being processed."""


def current_request() -> RequestMeta | None:
    """
    Returns information about the running Encore request,
    such as API calls and Pub/Sub messages being processed.

    Returns None only if no request is being processed,
    such as during system initialization.
    """
    req = get_current_request()
    if req is None:
        return None

    meta = req.meta() if hasattr(req, "meta") else req

    trace_data = None
    if meta.get("trace"):
        trace = meta["trace"]
        trace_data = TraceData(
            trace_id=trace.get("trace_id", ""),
            span_id=trace.get("span_id", ""),
            parent_trace_id=trace.get("parent_trace_id"),
            parent_span_id=trace.get("parent_span_id"),
            ext_correlation_id=trace.get("ext_correlation_id"),
        )

    if meta.get("api_call"):
        api_call = meta["api_call"]
        api = api_call.get("api", {})
        return APICallMeta(
            type="api-call",
            api=APIDesc(
                service=api.get("service", ""),
                endpoint=api.get("endpoint", ""),
                raw=api.get("raw", False),
                auth=api.get("requires_auth", False),
                tags=api.get("tags", []),
            ),
            method=api_call.get("method", "GET"),
            path=api_call.get("path", ""),
            path_and_query=api_call.get("path_and_query", ""),
            path_params=api_call.get("path_params", {}),
            headers=api_call.get("headers", {}),
            parsed_payload=api_call.get("parsed_payload"),
            middleware_data=meta.get("middleware_data"),
            trace=trace_data,
        )
    elif meta.get("pubsub_message"):
        msg = meta["pubsub_message"]
        return PubSubMessageMeta(
            type="pubsub-message",
            service=msg.get("service", ""),
            topic=msg.get("topic", ""),
            subscription=msg.get("subscription", ""),
            message_id=msg.get("id", ""),
            delivery_attempt=msg.get("delivery_attempt", 1),
            parsed_payload=msg.get("parsed_payload"),
            trace=trace_data,
        )

    return None


__all__ = [
    "APICallMeta",
    "APIDesc",
    "current_request",
    "Method",
    "PubSubMessageMeta",
    "RequestMeta",
    "TraceData",
]
