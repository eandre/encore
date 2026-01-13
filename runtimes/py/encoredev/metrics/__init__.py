"""
Metrics module for Encore applications.

This module provides counters and gauges for custom metrics
that can be statically analyzed by the Encore compiler and
automatically exported to observability backends.
"""

from __future__ import annotations

from dataclasses import dataclass, field
from typing import Any, Generic, TypeVar

from encoredev import current_request

Labels = TypeVar("Labels", bound=dict[str, Any])


@dataclass
class MetricConfig:
    """Configuration for metrics."""

    pass


class Counter:
    """
    A Counter tracks cumulative values that only increase.

    Use counters for metrics like request counts, errors, etc.

    Example:
        from encoredev.metrics import Counter

        orders_processed = Counter("orders_processed")

        orders_processed.increment()
        orders_processed.increment(5)
    """

    def __init__(self, name: str, config: MetricConfig | None = None) -> None:
        self._name = name
        self._config = config or MetricConfig()
        self._label_pairs: list[tuple[str, str]] = []
        self._values: dict[str, float] = {}

    def increment(self, value: float = 1) -> None:
        """Increment the counter by the given value (default 1)."""
        service_name = self._resolve_service_name()
        if service_name is None:
            return

        current = self._values.get(service_name, 0)
        self._values[service_name] = current + value

    def ref(self) -> "Counter":
        """Return a reference to this counter."""
        return self

    def _resolve_service_name(self) -> str | None:
        """Resolve the service name for this metric."""
        req = current_request()
        if req is not None:
            if req.type == "api-call":
                return req.api.service
            else:
                return req.service
        return None


class CounterGroup(Generic[Labels]):
    """
    A CounterGroup tracks counters with labels.

    Each unique combination of label values creates a separate counter time series.

    Example:
        from encoredev.metrics import CounterGroup
        from typing import TypedDict

        class Labels(TypedDict):
            success: bool
            method: str

        requests = CounterGroup[Labels]("http_requests")

        requests.with_({"success": True, "method": "GET"}).increment()
    """

    def __init__(self, name: str, config: MetricConfig | None = None) -> None:
        self._name = name
        self._config = config or MetricConfig()
        self._label_cache: dict[str, Counter] = {}

    def with_(self, labels: Labels) -> Counter:
        """
        Get a counter for the given label values.

        Note: Number values in labels are converted to integers using math.floor().
        """
        label_key = self._serialize_labels(labels)

        cached = self._label_cache.get(label_key)
        if cached is None:
            cached = Counter(self._name, self._config)
            cached._label_pairs = self._process_labels_to_pairs(labels)
            self._label_cache[label_key] = cached

        return cached

    def ref(self) -> "CounterGroup[Labels]":
        """Return a reference to this counter group."""
        return self

    def _serialize_labels(self, labels: Labels) -> str:
        """Serialize labels to a string key."""
        items = sorted(labels.items())  # type: ignore
        return ",".join(f"{k}={v}" for k, v in items)

    def _process_labels_to_pairs(self, labels: Labels) -> list[tuple[str, str]]:
        """Process labels to key-value pairs."""
        return [(str(k), str(v)) for k, v in sorted(labels.items())]  # type: ignore


class Gauge:
    """
    A Gauge tracks values that can go up or down.

    Use gauges for metrics like memory usage, active connections, temperature, etc.

    Example:
        from encoredev.metrics import Gauge

        active_connections = Gauge("active_connections")

        active_connections.set(10)
        active_connections.set(5)
    """

    def __init__(self, name: str, config: MetricConfig | None = None) -> None:
        self._name = name
        self._config = config or MetricConfig()
        self._label_pairs: list[tuple[str, str]] = []
        self._values: dict[str, float] = {}

    def set(self, value: float) -> None:
        """Set the gauge to the given value."""
        service_name = self._resolve_service_name()
        if service_name is None:
            return

        self._values[service_name] = value

    def ref(self) -> "Gauge":
        """Return a reference to this gauge."""
        return self

    def _resolve_service_name(self) -> str | None:
        """Resolve the service name for this metric."""
        req = current_request()
        if req is not None:
            if req.type == "api-call":
                return req.api.service
            else:
                return req.service
        return None


class GaugeGroup(Generic[Labels]):
    """
    A GaugeGroup tracks gauges with labels.

    Each unique combination of label values creates a separate gauge time series.

    Example:
        from encoredev.metrics import GaugeGroup
        from typing import TypedDict

        class Labels(TypedDict):
            region: str
            instance: str

        memory_usage = GaugeGroup[Labels]("memory_usage_bytes")

        memory_usage.with_({"region": "us-east", "instance": "i-123"}).set(1024)
    """

    def __init__(self, name: str, config: MetricConfig | None = None) -> None:
        self._name = name
        self._config = config or MetricConfig()
        self._label_cache: dict[str, Gauge] = {}

    def with_(self, labels: Labels) -> Gauge:
        """
        Get a gauge for the given label values.

        Note: Number values in labels are converted to integers using math.floor().
        """
        label_key = self._serialize_labels(labels)

        cached = self._label_cache.get(label_key)
        if cached is None:
            cached = Gauge(self._name, self._config)
            cached._label_pairs = self._process_labels_to_pairs(labels)
            self._label_cache[label_key] = cached

        return cached

    def ref(self) -> "GaugeGroup[Labels]":
        """Return a reference to this gauge group."""
        return self

    def _serialize_labels(self, labels: Labels) -> str:
        """Serialize labels to a string key."""
        items = sorted(labels.items())  # type: ignore
        return ",".join(f"{k}={v}" for k, v in items)

    def _process_labels_to_pairs(self, labels: Labels) -> list[tuple[str, str]]:
        """Process labels to key-value pairs."""
        return [(str(k), str(v)) for k, v in sorted(labels.items())]  # type: ignore


__all__ = [
    "Counter",
    "CounterGroup",
    "Gauge",
    "GaugeGroup",
    "MetricConfig",
]
