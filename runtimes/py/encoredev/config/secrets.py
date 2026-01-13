"""
Secrets management for Encore applications.

This module provides functionality for loading and accessing secrets.
"""

from __future__ import annotations

from typing import Generic, TypeVar

from encoredev.internal import RT

Name = TypeVar("Name", bound=str)


class Secret(Generic[Name]):
    """
    Secret represents a single secret value that is loaded into the application.

    It is strongly typed for that secret, so that you can write functions
    which expect a specific one.

    Example:
        def do_foo(s: Secret[Literal["foo"]]) -> None:
            foo = s()
    """

    def __init__(self, name: Name) -> None:
        self._name = name
        self._impl = RT.secret(name)

    def __call__(self) -> str:
        """
        Returns the current value of the secret.

        Encore will periodically refresh the value of the secret, so this
        value may change over time and could be stale for up to a couple of
        minutes.
        """
        if self._impl is None:
            raise ValueError(f"secret {self._name} is not set")
        return self._impl.cached()

    @property
    def name(self) -> Name:
        """The name of the secret."""
        return self._name

    def __str__(self) -> str:
        if self._impl is None:
            return f"Secret<{self._name}>(not set)"
        return f"Secret<{self._name}>(*********)"

    def __repr__(self) -> str:
        return self.__str__()


def secret(name: str) -> Secret[str]:
    """
    Load a single secret into the application.

    Example:
        from encoredev.config import secret

        api_key = secret("api_key")

        # Later use it:
        key = api_key()
    """
    return Secret(name)


__all__ = ["secret", "Secret"]
