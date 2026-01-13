"""
API error handling for Encore applications.

This module provides the APIError class and error codes for handling
errors in Encore API endpoints.
"""

from __future__ import annotations

from enum import Enum
from typing import Any


class ErrCode(str, Enum):
    """Error codes for API errors."""

    OK = "ok"
    """OK indicates the operation was successful."""

    CANCELED = "canceled"
    """
    Canceled indicates the operation was canceled (typically by the caller).
    Encore will generate this error code when cancellation is requested.
    """

    UNKNOWN = "unknown"
    """
    Unknown error. An example of where this error may be returned is
    if a Status value received from another address space belongs to
    an error-space that is not known in this address space.
    """

    INVALID_ARGUMENT = "invalid_argument"
    """
    InvalidArgument indicates client specified an invalid argument.
    Note that this differs from FailedPrecondition. It indicates arguments
    that are problematic regardless of the state of the system.
    """

    DEADLINE_EXCEEDED = "deadline_exceeded"
    """
    DeadlineExceeded means operation expired before completion.
    For operations that change the state of the system, this error may be
    returned even if the operation has completed successfully.
    """

    NOT_FOUND = "not_found"
    """
    NotFound means some requested entity (e.g., file or directory) was not found.
    """

    ALREADY_EXISTS = "already_exists"
    """
    AlreadyExists means an attempt to create an entity failed because one already exists.
    """

    PERMISSION_DENIED = "permission_denied"
    """
    PermissionDenied indicates the caller does not have permission to
    execute the specified operation.
    """

    RESOURCE_EXHAUSTED = "resource_exhausted"
    """
    ResourceExhausted indicates some resource has been exhausted, perhaps
    a per-user quota, or perhaps the entire file system is out of space.
    """

    FAILED_PRECONDITION = "failed_precondition"
    """
    FailedPrecondition indicates operation was rejected because the
    system is not in a state required for the operation's execution.
    """

    ABORTED = "aborted"
    """
    Aborted indicates the operation was aborted, typically due to a
    concurrency issue like sequencer check failures, transaction aborts, etc.
    """

    OUT_OF_RANGE = "out_of_range"
    """
    OutOfRange means operation was attempted past the valid range.
    E.g., seeking or reading past end of file.
    """

    UNIMPLEMENTED = "unimplemented"
    """
    Unimplemented indicates operation is not implemented or not
    supported/enabled in this service.
    """

    INTERNAL = "internal"
    """
    Internal errors. Means some invariants expected by underlying
    system has been broken.
    """

    UNAVAILABLE = "unavailable"
    """
    Unavailable indicates the service is currently unavailable.
    This is most likely a transient condition and may be corrected
    by retrying with a backoff.
    """

    DATA_LOSS = "data_loss"
    """
    DataLoss indicates unrecoverable data loss or corruption.
    """

    UNAUTHENTICATED = "unauthenticated"
    """
    Unauthenticated indicates the request does not have valid
    authentication credentials for the operation.
    """


class APIError(Exception):
    """
    API error with an error code and optional details.

    This error type is used to return structured errors from API endpoints
    with specific error codes that map to HTTP status codes.
    """

    def __init__(
        self,
        code: ErrCode,
        message: str,
        cause: BaseException | None = None,
        details: dict[str, Any] | None = None,
    ) -> None:
        super().__init__(message)
        self.code = code
        self.details = details
        self.__cause__ = cause

    def with_details(self, details: dict[str, Any]) -> "APIError":
        """Create a new APIError with the provided details."""
        return APIError(self.code, str(self), self.__cause__, details)

    @classmethod
    def canceled(cls, msg: str, cause: BaseException | None = None) -> "APIError":
        """Construct an APIError with the Canceled error code."""
        return cls(ErrCode.CANCELED, msg, cause)

    @classmethod
    def unknown(cls, msg: str, cause: BaseException | None = None) -> "APIError":
        """Construct an APIError with the Unknown error code."""
        return cls(ErrCode.UNKNOWN, msg, cause)

    @classmethod
    def invalid_argument(cls, msg: str, cause: BaseException | None = None) -> "APIError":
        """Construct an APIError with the InvalidArgument error code."""
        return cls(ErrCode.INVALID_ARGUMENT, msg, cause)

    @classmethod
    def deadline_exceeded(cls, msg: str, cause: BaseException | None = None) -> "APIError":
        """Construct an APIError with the DeadlineExceeded error code."""
        return cls(ErrCode.DEADLINE_EXCEEDED, msg, cause)

    @classmethod
    def not_found(cls, msg: str, cause: BaseException | None = None) -> "APIError":
        """Construct an APIError with the NotFound error code."""
        return cls(ErrCode.NOT_FOUND, msg, cause)

    @classmethod
    def already_exists(cls, msg: str, cause: BaseException | None = None) -> "APIError":
        """Construct an APIError with the AlreadyExists error code."""
        return cls(ErrCode.ALREADY_EXISTS, msg, cause)

    @classmethod
    def permission_denied(cls, msg: str, cause: BaseException | None = None) -> "APIError":
        """Construct an APIError with the PermissionDenied error code."""
        return cls(ErrCode.PERMISSION_DENIED, msg, cause)

    @classmethod
    def resource_exhausted(cls, msg: str, cause: BaseException | None = None) -> "APIError":
        """Construct an APIError with the ResourceExhausted error code."""
        return cls(ErrCode.RESOURCE_EXHAUSTED, msg, cause)

    @classmethod
    def failed_precondition(cls, msg: str, cause: BaseException | None = None) -> "APIError":
        """Construct an APIError with the FailedPrecondition error code."""
        return cls(ErrCode.FAILED_PRECONDITION, msg, cause)

    @classmethod
    def aborted(cls, msg: str, cause: BaseException | None = None) -> "APIError":
        """Construct an APIError with the Aborted error code."""
        return cls(ErrCode.ABORTED, msg, cause)

    @classmethod
    def out_of_range(cls, msg: str, cause: BaseException | None = None) -> "APIError":
        """Construct an APIError with the OutOfRange error code."""
        return cls(ErrCode.OUT_OF_RANGE, msg, cause)

    @classmethod
    def unimplemented(cls, msg: str, cause: BaseException | None = None) -> "APIError":
        """Construct an APIError with the Unimplemented error code."""
        return cls(ErrCode.UNIMPLEMENTED, msg, cause)

    @classmethod
    def internal(cls, msg: str, cause: BaseException | None = None) -> "APIError":
        """Construct an APIError with the Internal error code."""
        return cls(ErrCode.INTERNAL, msg, cause)

    @classmethod
    def unavailable(cls, msg: str, cause: BaseException | None = None) -> "APIError":
        """Construct an APIError with the Unavailable error code."""
        return cls(ErrCode.UNAVAILABLE, msg, cause)

    @classmethod
    def data_loss(cls, msg: str, cause: BaseException | None = None) -> "APIError":
        """Construct an APIError with the DataLoss error code."""
        return cls(ErrCode.DATA_LOSS, msg, cause)

    @classmethod
    def unauthenticated(cls, msg: str, cause: BaseException | None = None) -> "APIError":
        """Construct an APIError with the Unauthenticated error code."""
        return cls(ErrCode.UNAUTHENTICATED, msg, cause)


__all__ = ["APIError", "ErrCode"]
