"""
Logging module for Encore applications.

This module provides structured logging functionality for Encore applications.
"""

from __future__ import annotations

from enum import IntEnum
from typing import Any, Union

from encoredev.internal import RT, LogLevel as InternalLogLevel
from encoredev.internal.runtime import get_current_request


# Field types that can be logged
FieldValue = Union[str, int, float, bool, None, dict[str, Any], list[Any]]
FieldsObject = dict[str, FieldValue]


class LogLevel(IntEnum):
    """Log levels for the logger."""

    # Values match the native runtime (1-5)
    TRACE = 1
    DEBUG = 2
    INFO = 3
    WARN = 4
    ERROR = 5


class Logger:
    """
    Structured logger for Encore applications.

    Provides methods for logging at different levels with structured fields.
    """

    def __init__(self, impl: Any = None, level: LogLevel = LogLevel.INFO) -> None:
        if impl is None:
            self._impl = RT.logger()
        else:
            self._impl = impl
        self._level = level

    def with_level(self, level: LogLevel) -> "Logger":
        """Returns a new logger with the specified level."""
        new_impl = self._impl.with_level(int(level))
        return Logger(new_impl, level)

    def with_fields(self, fields: FieldsObject) -> "Logger":
        """Returns a new logger with the given fields added to the context."""
        new_impl = self._impl.with_fields(fields)
        return Logger(new_impl, self._level)

    # Alias for compatibility with JS API
    def with_(self, fields: FieldsObject) -> "Logger":
        """Returns a new logger with the given fields added to the context."""
        return self.with_fields(fields)

    def trace(self, msg: str, fields: FieldsObject | None = None) -> None:
        """Log a message at the trace level."""
        self._log(LogLevel.TRACE, msg, fields=fields)

    def debug(self, msg: str, fields: FieldsObject | None = None) -> None:
        """Log a message at the debug level."""
        self._log(LogLevel.DEBUG, msg, fields=fields)

    def info(self, msg: str, fields: FieldsObject | None = None) -> None:
        """Log a message at the info level."""
        self._log(LogLevel.INFO, msg, fields=fields)

    def warn(
        self,
        err_or_msg: BaseException | str,
        msg_or_fields: str | FieldsObject | None = None,
        fields: FieldsObject | None = None,
    ) -> None:
        """
        Log a message at the warn level.

        Supports multiple signatures:
        - warn(msg)
        - warn(msg, fields)
        - warn(err, msg)
        - warn(err, msg, fields)
        - warn(err, fields)
        """
        self._log_with_error(LogLevel.WARN, err_or_msg, msg_or_fields, fields)

    def error(
        self,
        err_or_msg: BaseException | str,
        msg_or_fields: str | FieldsObject | None = None,
        fields: FieldsObject | None = None,
    ) -> None:
        """
        Log a message at the error level.

        Supports multiple signatures:
        - error(msg)
        - error(msg, fields)
        - error(err, msg)
        - error(err, msg, fields)
        - error(err, fields)
        """
        self._log_with_error(LogLevel.ERROR, err_or_msg, msg_or_fields, fields)

    def _log(
        self,
        level: LogLevel,
        msg: str,
        error: BaseException | None = None,
        fields: FieldsObject | None = None,
    ) -> None:
        """Internal logging implementation."""
        req = get_current_request()
        self._impl.log(req, int(level), msg, error, None, fields)

    def _log_with_error(
        self,
        level: LogLevel,
        err_or_msg: BaseException | str,
        msg_or_fields: str | FieldsObject | None = None,
        fields: FieldsObject | None = None,
    ) -> None:
        """Internal logging implementation with error handling."""
        err: BaseException | None = None
        msg: str

        if isinstance(err_or_msg, str):
            # log(msg) or log(msg, fields)
            msg = err_or_msg
            if isinstance(msg_or_fields, dict):
                fields = msg_or_fields
        elif isinstance(msg_or_fields, str):
            # log(err, msg) or log(err, msg, fields)
            err = err_or_msg
            msg = msg_or_fields
        else:
            # log(err) or log(err, fields)
            err = err_or_msg
            msg = ""
            if isinstance(msg_or_fields, dict):
                fields = msg_or_fields

        self._log(level, msg, err, fields)


# Default logger instance
log = Logger()


def trace(msg: str, fields: FieldsObject | None = None) -> None:
    """Log a message at the trace level."""
    log.trace(msg, fields)


def debug(msg: str, fields: FieldsObject | None = None) -> None:
    """Log a message at the debug level."""
    log.debug(msg, fields)


def info(msg: str, fields: FieldsObject | None = None) -> None:
    """Log a message at the info level."""
    log.info(msg, fields)


def warn(
    err_or_msg: BaseException | str,
    msg_or_fields: str | FieldsObject | None = None,
    fields: FieldsObject | None = None,
) -> None:
    """Log a message at the warn level."""
    log.warn(err_or_msg, msg_or_fields, fields)


def error(
    err_or_msg: BaseException | str,
    msg_or_fields: str | FieldsObject | None = None,
    fields: FieldsObject | None = None,
) -> None:
    """Log a message at the error level."""
    log.error(err_or_msg, msg_or_fields, fields)


__all__ = [
    "debug",
    "error",
    "FieldsObject",
    "FieldValue",
    "info",
    "log",
    "Logger",
    "LogLevel",
    "trace",
    "warn",
]
