"""
Streaming support for Encore applications.

This module provides classes for bidirectional streaming in API endpoints.
"""

from __future__ import annotations

from typing import AsyncIterator, Generic, Protocol, TypeVar

Request = TypeVar("Request")
Response = TypeVar("Response")


class StreamIn(Protocol, Generic[Request]):
    """Input stream interface for receiving messages."""

    async def recv(self) -> Request:
        """Receive the next message from the stream."""
        ...

    def __aiter__(self) -> AsyncIterator[Request]:
        """Iterate over messages in the stream."""
        ...

    async def __anext__(self) -> Request:
        """Get the next message in the iteration."""
        ...


class StreamOut(Protocol, Generic[Response]):
    """Output stream interface for sending messages."""

    async def send(self, msg: Response) -> None:
        """Send a message to the stream."""
        ...

    async def close(self) -> None:
        """Close the stream."""
        ...


class StreamInOut(StreamIn[Request], StreamOut[Response], Generic[Request, Response]):
    """Bidirectional stream interface for sending and receiving messages."""

    pass


class StreamOutWithResponse(StreamOut[Request], Generic[Request, Response]):
    """Output stream that also provides a response."""

    async def response(self) -> Response:
        """Get the response from the stream."""
        ...


__all__ = [
    "StreamIn",
    "StreamInOut",
    "StreamOut",
    "StreamOutWithResponse",
]
