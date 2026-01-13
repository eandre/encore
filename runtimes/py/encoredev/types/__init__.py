"""
Custom types for Encore applications.

This module provides custom types like Decimal for arbitrary precision arithmetic.
"""

from __future__ import annotations

from decimal import Decimal as StdDecimal
from typing import Union


ToDecimal = Union[str, int, float]
"""Types that can be converted to a Decimal."""


class Decimal:
    """
    A decimal type that can hold values with arbitrary precision.

    Unlike Python's native float type, this can accurately represent
    decimal values without floating-point precision errors.

    Example:
        from encoredev.types import Decimal

        price = Decimal("19.99")
        quantity = Decimal(3)
        total = price.mul(quantity)  # Decimal("59.97")
    """

    def __init__(self, value: ToDecimal) -> None:
        self._impl = StdDecimal(str(value))

    @classmethod
    def _from_impl(cls, impl: StdDecimal) -> "Decimal":
        """Create a Decimal from an internal implementation."""
        d = object.__new__(cls)
        d._impl = impl
        return d

    def _to_impl(self, value: "Decimal | ToDecimal") -> StdDecimal:
        """Convert a value to internal implementation."""
        if isinstance(value, Decimal):
            return value._impl
        return StdDecimal(str(value))

    def add(self, d: "Decimal | ToDecimal") -> "Decimal":
        """Add this decimal to another decimal value."""
        return Decimal._from_impl(self._impl + self._to_impl(d))

    def sub(self, d: "Decimal | ToDecimal") -> "Decimal":
        """Subtract another decimal value from this decimal."""
        return Decimal._from_impl(self._impl - self._to_impl(d))

    def mul(self, d: "Decimal | ToDecimal") -> "Decimal":
        """Multiply this decimal by another decimal value."""
        return Decimal._from_impl(self._impl * self._to_impl(d))

    def div(self, d: "Decimal | ToDecimal") -> "Decimal":
        """Divide this decimal by another decimal value."""
        return Decimal._from_impl(self._impl / self._to_impl(d))

    @property
    def value(self) -> str:
        """Get the string representation of the decimal value."""
        return str(self._impl)

    def __str__(self) -> str:
        return str(self._impl)

    def __repr__(self) -> str:
        return f"Decimal({self._impl!r})"

    def __eq__(self, other: object) -> bool:
        if isinstance(other, Decimal):
            return self._impl == other._impl
        if isinstance(other, (str, int, float)):
            return self._impl == StdDecimal(str(other))
        return NotImplemented

    def __lt__(self, other: "Decimal | ToDecimal") -> bool:
        return self._impl < self._to_impl(other)

    def __le__(self, other: "Decimal | ToDecimal") -> bool:
        return self._impl <= self._to_impl(other)

    def __gt__(self, other: "Decimal | ToDecimal") -> bool:
        return self._impl > self._to_impl(other)

    def __ge__(self, other: "Decimal | ToDecimal") -> bool:
        return self._impl >= self._to_impl(other)

    def __hash__(self) -> int:
        return hash(self._impl)

    def __float__(self) -> float:
        return float(self._impl)

    def __int__(self) -> int:
        return int(self._impl)

    def __add__(self, other: "Decimal | ToDecimal") -> "Decimal":
        return self.add(other)

    def __radd__(self, other: "Decimal | ToDecimal") -> "Decimal":
        return self.add(other)

    def __sub__(self, other: "Decimal | ToDecimal") -> "Decimal":
        return self.sub(other)

    def __rsub__(self, other: "Decimal | ToDecimal") -> "Decimal":
        return Decimal._from_impl(self._to_impl(other) - self._impl)

    def __mul__(self, other: "Decimal | ToDecimal") -> "Decimal":
        return self.mul(other)

    def __rmul__(self, other: "Decimal | ToDecimal") -> "Decimal":
        return self.mul(other)

    def __truediv__(self, other: "Decimal | ToDecimal") -> "Decimal":
        return self.div(other)

    def __rtruediv__(self, other: "Decimal | ToDecimal") -> "Decimal":
        return Decimal._from_impl(self._to_impl(other) / self._impl)

    def __neg__(self) -> "Decimal":
        return Decimal._from_impl(-self._impl)

    def __pos__(self) -> "Decimal":
        return Decimal._from_impl(+self._impl)

    def __abs__(self) -> "Decimal":
        return Decimal._from_impl(abs(self._impl))


__all__ = ["Decimal", "ToDecimal"]
