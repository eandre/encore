"""
Object storage module for Encore applications.

This module provides functionality for object storage (buckets).
"""

from __future__ import annotations

from dataclasses import dataclass
from typing import Any, AsyncGenerator

from encoredev.internal import RT
from encoredev.internal.runtime import get_current_request


class ObjectsError(Exception):
    """Base error for object storage operations."""

    pass


class ObjectNotFound(ObjectsError):
    """Error raised when an object is not found."""

    pass


class PreconditionFailed(ObjectsError):
    """Error raised when a precondition fails."""

    pass


class InvalidArgument(ObjectsError):
    """Error raised when an argument is invalid."""

    pass


@dataclass
class BucketConfig:
    """Configuration for a storage bucket."""

    public: bool = False
    """Whether the objects in the bucket should be publicly accessible via CDN."""

    versioned: bool = False
    """Whether to enable versioning of the objects in the bucket."""


@dataclass
class ObjectAttrs:
    """Attributes of an object in a bucket."""

    name: str
    """The name of the object."""

    size: int
    """The size of the object in bytes."""

    etag: str
    """The ETag of the object."""

    version: str | None = None
    """The version of the object, if bucket versioning is enabled."""

    content_type: str | None = None
    """The content type of the object."""


@dataclass
class ListEntry:
    """Entry in a bucket listing."""

    name: str
    """The name of the object."""

    size: int
    """The size of the object in bytes."""

    etag: str
    """The ETag of the object."""


@dataclass
class ListOptions:
    """Options for listing objects in a bucket."""

    prefix: str | None = None
    """Only include objects with this prefix in the listing."""

    limit: int | None = None
    """Maximum number of objects to return."""


@dataclass
class AttrsOptions:
    """Options for getting object attributes."""

    version: str | None = None
    """The object version to retrieve attributes for."""


@dataclass
class ExistsOptions:
    """Options for checking object existence."""

    version: str | None = None
    """The object version to check for existence."""


@dataclass
class DeleteOptions:
    """Options for deleting an object."""

    version: str | None = None
    """The object version to delete."""


@dataclass
class DownloadOptions:
    """Options for downloading an object."""

    version: str | None = None
    """The object version to download."""


@dataclass
class UploadOptions:
    """Options for uploading an object."""

    content_type: str | None = None
    """The content type of the object."""

    preconditions: "UploadPreconditions | None" = None
    """Preconditions for the upload."""


@dataclass
class UploadPreconditions:
    """Preconditions for uploading an object."""

    not_exists: bool = False
    """Only upload if the object does not already exist."""


@dataclass
class UploadUrlOptions:
    """Options for generating a signed upload URL."""

    ttl: int | None = None
    """
    The expiration time of the URL, in seconds from signing.
    Maximum value is seven days. Default is one hour.
    """


@dataclass
class SignedUploadUrl:
    """Signed upload URL response."""

    url: str
    """The signed upload URL."""


@dataclass
class DownloadUrlOptions:
    """Options for generating a signed download URL."""

    ttl: int | None = None
    """
    The expiration time of the URL, in seconds from signing.
    Maximum value is seven days. Default is one hour.
    """


@dataclass
class SignedDownloadUrl:
    """Signed download URL response."""

    url: str
    """The signed download URL."""


class BucketPerms:
    """Base class for bucket permissions."""

    pass


class Uploader(BucketPerms):
    """Interface for uploading objects."""

    async def upload(
        self, name: str, data: bytes, options: UploadOptions | None = None
    ) -> ObjectAttrs:
        """Upload an object to the bucket."""
        raise NotImplementedError


class SignedUploader(BucketPerms):
    """Interface for generating signed upload URLs."""

    async def signed_upload_url(
        self, name: str, options: UploadUrlOptions | None = None
    ) -> SignedUploadUrl:
        """Generate a signed upload URL."""
        raise NotImplementedError


class Downloader(BucketPerms):
    """Interface for downloading objects."""

    async def download(
        self, name: str, options: DownloadOptions | None = None
    ) -> bytes:
        """Download an object from the bucket."""
        raise NotImplementedError


class SignedDownloader(BucketPerms):
    """Interface for generating signed download URLs."""

    async def signed_download_url(
        self, name: str, options: DownloadUrlOptions | None = None
    ) -> SignedDownloadUrl:
        """Generate a signed download URL."""
        raise NotImplementedError


class Attrser(BucketPerms):
    """Interface for getting object attributes."""

    async def attrs(
        self, name: str, options: AttrsOptions | None = None
    ) -> ObjectAttrs:
        """Get the object's attributes."""
        raise NotImplementedError

    async def exists(
        self, name: str, options: ExistsOptions | None = None
    ) -> bool:
        """Check if the object exists."""
        raise NotImplementedError


class Lister(BucketPerms):
    """Interface for listing objects."""

    async def list(self, options: ListOptions) -> AsyncGenerator[ListEntry, None]:
        """List objects in the bucket."""
        raise NotImplementedError
        yield  # type: ignore


class Remover(BucketPerms):
    """Interface for removing objects."""

    async def remove(
        self, name: str, options: DeleteOptions | None = None
    ) -> None:
        """Remove an object from the bucket."""
        raise NotImplementedError


class PublicUrler(BucketPerms):
    """Interface for getting public URLs."""

    def public_url(self, name: str) -> str:
        """Get the public URL for an object."""
        raise NotImplementedError


class ReadWriter(
    Uploader,
    SignedUploader,
    Downloader,
    SignedDownloader,
    Attrser,
    Lister,
    Remover,
):
    """Combined interface for full bucket access."""

    pass


def _options_to_dict(options: Any | None) -> dict[str, Any] | None:
    """Convert options dataclass to dict."""
    if options is None:
        return None
    return {k: v for k, v in options.__dict__.items() if v is not None}


class Bucket(
    Uploader,
    SignedUploader,
    Downloader,
    SignedDownloader,
    Attrser,
    Lister,
    Remover,
    PublicUrler,
):
    """
    Defines a new Object Storage bucket infrastructure resource.

    Example:
        from encoredev.storage.objects import Bucket, BucketConfig

        images = Bucket(
            "images",
            BucketConfig(public=True, versioned=False)
        )

        # Upload an object
        await images.upload("photo.jpg", image_data, UploadOptions(content_type="image/jpeg"))

        # Download an object
        data = await images.download("photo.jpg")

        # List objects
        async for entry in images.list(ListOptions(prefix="photos/")):
            print(entry.name)
    """

    def __init__(self, name: str, config: BucketConfig | None = None) -> None:
        self._name = name
        self._config = config or BucketConfig()
        self._impl = RT.bucket(name)

    @classmethod
    def named(cls, name: str) -> "Bucket":
        """
        Reference an existing bucket by name.

        To create a new storage bucket, use `Bucket(name)` instead.
        """
        return cls(name)

    async def list(self, options: ListOptions) -> AsyncGenerator[ListEntry, None]:
        """List objects in the bucket."""
        source = get_current_request()
        opts = _options_to_dict(options)
        iterator = await self._impl.list(opts, source)
        while True:
            entry = await iterator.next()
            if entry is None:
                iterator.mark_done()
                break
            yield ListEntry(
                name=entry.get("name", ""),
                size=entry.get("size", 0),
                etag=entry.get("etag", ""),
            )

    async def exists(
        self, name: str, options: ExistsOptions | None = None
    ) -> bool:
        """Check if the object exists in the bucket."""
        source = get_current_request()
        obj = self._impl.object(name)
        opts = _options_to_dict(options)
        return await obj.exists(opts, source)

    async def attrs(
        self, name: str, options: AttrsOptions | None = None
    ) -> ObjectAttrs:
        """Get the object's attributes."""
        source = get_current_request()
        obj = self._impl.object(name)
        opts = _options_to_dict(options)
        result = await obj.attrs(opts, source)
        return ObjectAttrs(
            name=result.get("name", name),
            size=result.get("size", 0),
            etag=result.get("etag", ""),
            version=result.get("version"),
            content_type=result.get("content_type"),
        )

    async def upload(
        self, name: str, data: bytes, options: UploadOptions | None = None
    ) -> ObjectAttrs:
        """Upload an object to the bucket."""
        source = get_current_request()
        obj = self._impl.object(name)
        opts = _options_to_dict(options)
        result = await obj.upload(data, opts, source)
        return ObjectAttrs(
            name=result.get("name", name),
            size=result.get("size", len(data)),
            etag=result.get("etag", ""),
            version=result.get("version"),
            content_type=result.get("content_type"),
        )

    async def signed_upload_url(
        self, name: str, options: UploadUrlOptions | None = None
    ) -> SignedUploadUrl:
        """Generate a signed upload URL."""
        source = get_current_request()
        obj = self._impl.object(name)
        opts = _options_to_dict(options)
        result = await obj.signed_upload_url(opts, source)
        return SignedUploadUrl(url=result.get("url", ""))

    async def signed_download_url(
        self, name: str, options: DownloadUrlOptions | None = None
    ) -> SignedDownloadUrl:
        """Generate a signed download URL."""
        source = get_current_request()
        obj = self._impl.object(name)
        opts = _options_to_dict(options)
        result = await obj.signed_download_url(opts, source)
        return SignedDownloadUrl(url=result.get("url", ""))

    async def download(
        self, name: str, options: DownloadOptions | None = None
    ) -> bytes:
        """Download an object from the bucket."""
        source = get_current_request()
        obj = self._impl.object(name)
        opts = _options_to_dict(options)
        return await obj.download_all(opts, source)

    async def remove(
        self, name: str, options: DeleteOptions | None = None
    ) -> None:
        """Remove an object from the bucket."""
        source = get_current_request()
        obj = self._impl.object(name)
        opts = _options_to_dict(options)
        await obj.delete(opts, source)

    def public_url(self, name: str) -> str:
        """Get the public URL for an object."""
        obj = self._impl.object(name)
        return obj.public_url()

    def ref(self) -> "Bucket":
        """Return a reference to this bucket."""
        return self


__all__ = [
    "Attrser",
    "AttrsOptions",
    "Bucket",
    "BucketConfig",
    "BucketPerms",
    "DeleteOptions",
    "DownloadOptions",
    "DownloadUrlOptions",
    "Downloader",
    "ExistsOptions",
    "InvalidArgument",
    "ListEntry",
    "Lister",
    "ListOptions",
    "ObjectAttrs",
    "ObjectNotFound",
    "ObjectsError",
    "PreconditionFailed",
    "PublicUrler",
    "ReadWriter",
    "Remover",
    "SignedDownloader",
    "SignedDownloadUrl",
    "SignedUploader",
    "SignedUploadUrl",
    "UploadOptions",
    "UploadPreconditions",
    "Uploader",
    "UploadUrlOptions",
]
