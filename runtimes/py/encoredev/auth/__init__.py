"""
Authentication module for Encore applications.

This module provides functionality for defining authentication handlers.
"""

from __future__ import annotations

from typing import Awaitable, Callable, Generic, Protocol, TypeVar

Params = TypeVar("Params", bound=object)
AuthData = TypeVar("AuthData", bound=object)


class AuthDataProtocol(Protocol):
    """Protocol for auth data that must have a user_id."""

    user_id: str


class AuthHandler(Generic[Params, AuthData]):
    """
    Authentication handler type.

    An auth handler receives authentication parameters and returns
    authentication data (which must include at least a user_id) or None
    if authentication fails.
    """

    def __init__(
        self,
        fn: Callable[[Params], Awaitable[AuthData | None]],
    ) -> None:
        self._fn = fn

    async def __call__(self, params: Params) -> AuthData | None:
        """Call the auth handler."""
        return await self._fn(params)


def auth_handler(
    fn: Callable[[Params], Awaitable[AuthData | None]],
) -> AuthHandler[Params, AuthData]:
    """
    Create a type-safe authentication handler.

    The auth data returned must have at minimum a `user_id` field.

    Example:
        @dataclass
        class AuthParams:
            token: str

        @dataclass
        class UserData:
            user_id: str
            email: str

        @auth_handler
        async def my_auth(params: AuthParams) -> UserData | None:
            # Validate token and return user data
            user = await validate_token(params.token)
            if user:
                return UserData(user_id=user.id, email=user.email)
            return None
    """
    return AuthHandler(fn)


__all__ = ["auth_handler", "AuthHandler"]
