"""Application initialization module for Encore runtime."""

from __future__ import annotations

import functools
import inspect
from typing import Any, Callable

from encoredev.internal import RT, APIRoute, set_current_request


def register_handlers(handlers: list[dict[str, Any]]) -> None:
    """Register API handlers with the runtime, transforming them to handle Request unwrapping."""
    routes = []
    for h in handlers:
        wrapped_handler = transform_handler(h["handler"])
        route = APIRoute(
            service=h["service"],
            name=h["name"],
            handler=wrapped_handler,
            raw=h.get("raw", False),
            streaming=h.get("streaming", False),
        )
        routes.append(route)
    RT.register_handlers(routes)


def transform_handler(handler: Callable[..., Any]) -> Callable[..., Any]:
    """Wrap a handler to extract payload from Request object."""
    if inspect.iscoroutinefunction(handler):

        @functools.wraps(handler)
        async def async_wrapper(req: Any) -> Any:
            set_current_request(req)
            payload = req.payload() if hasattr(req, "payload") else req
            if payload is None:
                return await handler()
            return await handler(payload)

        return async_wrapper
    else:

        @functools.wraps(handler)
        def sync_wrapper(req: Any) -> Any:
            set_current_request(req)
            payload = req.payload() if hasattr(req, "payload") else req
            if payload is None:
                return handler()
            return handler(payload)

        return sync_wrapper


def run() -> None:
    """Run the Encore runtime."""
    RT.run_forever()
