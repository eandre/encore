"""
Validation module for Encore applications.

This module provides type markers for validation constraints.
These are compile-time markers that have no runtime representation
but are used by the Encore compiler for validation.

In Python, these are implemented using typing.Annotated with special
marker classes that the Encore compiler can recognize.
"""

from __future__ import annotations

from typing import Annotated, Generic, TypeVar

T = TypeVar("T")
N = TypeVar("N", bound=int)
S = TypeVar("S", bound=str)


class _ValidationMarker:
    """Base class for validation markers."""

    pass


class _Min(_ValidationMarker, Generic[N]):
    """Marker for minimum numeric value."""

    def __init__(self, value: int) -> None:
        self.min_value = value


class _Max(_ValidationMarker, Generic[N]):
    """Marker for maximum numeric value."""

    def __init__(self, value: int) -> None:
        self.max_value = value


class _MinLen(_ValidationMarker, Generic[N]):
    """Marker for minimum string length."""

    def __init__(self, value: int) -> None:
        self.min_len = value


class _MaxLen(_ValidationMarker, Generic[N]):
    """Marker for maximum string length."""

    def __init__(self, value: int) -> None:
        self.max_len = value


class _MatchesRegexp(_ValidationMarker, Generic[S]):
    """Marker for regex pattern matching."""

    def __init__(self, pattern: str) -> None:
        self.matches_regexp = pattern


class _StartsWith(_ValidationMarker, Generic[S]):
    """Marker for string prefix matching."""

    def __init__(self, prefix: str) -> None:
        self.starts_with = prefix


class _EndsWith(_ValidationMarker, Generic[S]):
    """Marker for string suffix matching."""

    def __init__(self, suffix: str) -> None:
        self.ends_with = suffix


class _IsEmail(_ValidationMarker):
    """Marker for email validation."""

    is_email = True


class _IsURL(_ValidationMarker):
    """Marker for URL validation."""

    is_url = True


def Min(value: int) -> type[_Min[int]]:
    """
    Validation constraint for minimum numeric value.

    Example:
        from typing import Annotated
        from encoredev.validate import Min

        class Request:
            age: Annotated[int, Min(0)]  # Must be >= 0
    """
    return type("Min", (_Min,), {"min_value": value})


def Max(value: int) -> type[_Max[int]]:
    """
    Validation constraint for maximum numeric value.

    Example:
        from typing import Annotated
        from encoredev.validate import Max

        class Request:
            count: Annotated[int, Max(100)]  # Must be <= 100
    """
    return type("Max", (_Max,), {"max_value": value})


def MinLen(value: int) -> type[_MinLen[int]]:
    """
    Validation constraint for minimum string length.

    Example:
        from typing import Annotated
        from encoredev.validate import MinLen

        class Request:
            name: Annotated[str, MinLen(1)]  # Must have at least 1 character
    """
    return type("MinLen", (_MinLen,), {"min_len": value})


def MaxLen(value: int) -> type[_MaxLen[int]]:
    """
    Validation constraint for maximum string length.

    Example:
        from typing import Annotated
        from encoredev.validate import MaxLen

        class Request:
            name: Annotated[str, MaxLen(100)]  # Must have at most 100 characters
    """
    return type("MaxLen", (_MaxLen,), {"max_len": value})


def MatchesRegexp(pattern: str) -> type[_MatchesRegexp[str]]:
    """
    Validation constraint for regex pattern matching.

    Example:
        from typing import Annotated
        from encoredev.validate import MatchesRegexp

        class Request:
            code: Annotated[str, MatchesRegexp(r"^[A-Z]{3}$")]  # Must be 3 uppercase letters
    """
    return type("MatchesRegexp", (_MatchesRegexp,), {"matches_regexp": pattern})


def StartsWith(prefix: str) -> type[_StartsWith[str]]:
    """
    Validation constraint for string prefix.

    Example:
        from typing import Annotated
        from encoredev.validate import StartsWith

        class Request:
            id: Annotated[str, StartsWith("usr_")]  # Must start with "usr_"
    """
    return type("StartsWith", (_StartsWith,), {"starts_with": prefix})


def EndsWith(suffix: str) -> type[_EndsWith[str]]:
    """
    Validation constraint for string suffix.

    Example:
        from typing import Annotated
        from encoredev.validate import EndsWith

        class Request:
            filename: Annotated[str, EndsWith(".txt")]  # Must end with ".txt"
    """
    return type("EndsWith", (_EndsWith,), {"ends_with": suffix})


# Singleton instances for boolean markers
IsEmail = _IsEmail
"""
Validation constraint for email format.

Example:
    from typing import Annotated
    from encoredev.validate import IsEmail

    class Request:
        email: Annotated[str, IsEmail]  # Must be a valid email
"""

IsURL = _IsURL
"""
Validation constraint for URL format.

Example:
    from typing import Annotated
    from encoredev.validate import IsURL

    class Request:
        website: Annotated[str, IsURL]  # Must be a valid URL
"""


__all__ = [
    "EndsWith",
    "IsEmail",
    "IsURL",
    "MatchesRegexp",
    "Max",
    "MaxLen",
    "Min",
    "MinLen",
    "StartsWith",
]
