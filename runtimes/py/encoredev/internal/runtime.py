"""
Runtime bindings for the Encore runtime.

This module provides the interface to the Encore runtime, which is implemented
in Rust and exposed via PyO3. The actual implementation is in the encoredev._runtime
native module.
"""

from __future__ import annotations

import asyncio
import os
from contextvars import ContextVar
from typing import Any

from encoredev import _runtime

# Re-export all native types
from encoredev._runtime import (
    APIError,
    APIRoute,
    AppMeta,
    Bucket,
    BucketObject,
    BuildMeta,
    CloudProvider,
    Cursor,
    DeployMeta,
    EnvironmentMeta,
    EnvironmentType,
    Gateway,
    HostedService,
    ListIterator,
    Logger,
    LogLevel,
    Metric,
    ObjectAttrs,
    PubSubSubscription,
    PubSubSubscriptionConfig,
    PubSubTopic,
    QueryArgs,
    Request,
    Row,
    Runtime,
    RuntimeConfig,
    Secret,
    SQLDatabase,
    Transaction,
)

# Context variable for tracking the current request
_current_request: ContextVar[Any | None] = ContextVar("current_request", default=None)


def get_current_request() -> Any | None:
    """Get the current request from the context."""
    return _current_request.get()


def set_current_request(request: Any | None) -> None:
    """Set the current request in the context."""
    _current_request.set(request)


# Check if we're in test mode
_test_mode = os.environ.get("PYTHON_ENV") == "test"


class GlobalEventLoopPolicy(asyncio.DefaultEventLoopPolicy):
    def __init__(self, loop):
        super().__init__()
        self._loop = loop

    def get_event_loop(self):
        return self._loop


global_loop = asyncio.new_event_loop()
asyncio.set_event_loop_policy(GlobalEventLoopPolicy(global_loop))


# Global runtime instance (singleton)
RT = Runtime(event_loop=global_loop, test_mode=_test_mode)


def version() -> str:
    """Get the runtime version."""
    return _runtime.version()


def build_commit() -> str:
    """Get the runtime build commit."""
    return _runtime.build_commit()


# Cached runtime config
_cached_config: dict[str, Any] | None = None


def runtime_config() -> dict[str, Any]:
    """Get the runtime configuration."""
    global _cached_config
    if _cached_config is None:
        cfg = RT.runtime_config()
        _cached_config = {"metrics": cfg.metrics}
    return _cached_config


__all__ = [
    # Singleton
    "RT",
    # Functions
    "version",
    "build_commit",
    "runtime_config",
    "get_current_request",
    "set_current_request",
    # Native types
    "Runtime",
    "Request",
    "APIRoute",
    "APIError",
    "Logger",
    "LogLevel",
    "EnvironmentType",
    "CloudProvider",
    "PubSubTopic",
    "PubSubSubscription",
    "PubSubSubscriptionConfig",
    "SQLDatabase",
    "QueryArgs",
    "Cursor",
    "Row",
    "Transaction",
    "Bucket",
    "BucketObject",
    "ListIterator",
    "ObjectAttrs",
    "Gateway",
    "Secret",
    "AppMeta",
    "EnvironmentMeta",
    "BuildMeta",
    "DeployMeta",
    "HostedService",
    "RuntimeConfig",
    "Metric",
]
