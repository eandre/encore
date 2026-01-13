# Basic object storage bucket definitions

from encoredev.storage.objects import Bucket

# Simple bucket with empty config
uploads = Bucket("uploads", {})

# Public bucket
public_assets = Bucket("public-assets", {
    "public": True,
})

# Versioned bucket
documents = Bucket("documents", {
    "versioned": True,
})

# Public and versioned bucket
media = Bucket("media", {
    "public": True,
    "versioned": True,
})
