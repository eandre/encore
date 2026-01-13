"""
API Gateway for Encore applications.

This module provides the Gateway class for configuring the API gateway.
"""

from __future__ import annotations

from dataclasses import dataclass
from typing import Any, Callable, TYPE_CHECKING

from encoredev.internal import RT
from encoredev.internal.runtime import set_current_request

if TYPE_CHECKING:
    from encoredev.auth import AuthHandler


@dataclass
class GatewayConfig:
    """Configuration for the API gateway."""

    auth_handler: "AuthHandler[Any, Any] | None" = None
    """The authentication handler for the gateway."""


class Gateway:
    """
    API Gateway for routing and authentication.

    The gateway handles routing requests to the appropriate service
    and managing authentication.
    """

    def __init__(self, config: GatewayConfig | None = None) -> None:
        self.name = "api-gateway"
        self.config = config or GatewayConfig()

        auth = None
        if self.config.auth_handler:
            handler = self.config.auth_handler

            def auth_wrapper(req: Any) -> Any:
                set_current_request(req)
                payload = req.payload() if hasattr(req, "payload") else req
                return handler(payload)

            auth = auth_wrapper

        self._impl = RT.gateway("api-gateway", {"auth": auth})


__all__ = ["Gateway", "GatewayConfig"]
