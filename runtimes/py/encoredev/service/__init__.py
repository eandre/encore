"""
Service module for Encore applications.

This module provides functionality for defining Encore backend services.
"""

from __future__ import annotations

from dataclasses import dataclass, field
from typing import TYPE_CHECKING

if TYPE_CHECKING:
    from encoredev.api import Middleware


@dataclass
class ServiceConfig:
    """Configuration for a service."""

    middlewares: list["Middleware"] = field(default_factory=list)
    """List of middlewares to apply to all endpoints in the service."""


class Service:
    """
    Defines an Encore backend service.

    Use this class to define a new backend service with the given name.
    The scope of the service is its containing directory, and all subdirectories.

    It must be called from files named `encore.service.py`, to enable Encore to
    efficiently identify possible service definitions.

    Example:
        from encoredev.service import Service, ServiceConfig

        svc = Service(
            "my-service",
            ServiceConfig(middlewares=[auth_middleware])
        )
    """

    def __init__(self, name: str, config: ServiceConfig | None = None) -> None:
        self.name = name
        self.config = config or ServiceConfig()


__all__ = ["Service", "ServiceConfig"]
