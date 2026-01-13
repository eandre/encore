"""
Encore SDK for Python.

This module provides the core functionality for building Encore applications in Python.
"""

from encoredev.app_meta import app_meta, AppMeta, BuildMeta, DeployMeta, EnvironmentMeta
from encoredev.app_meta import EnvironmentType, CloudProvider, HostedService
from encoredev.req_meta import current_request, RequestMeta, APICallMeta, PubSubMessageMeta
from encoredev.req_meta import APIDesc, Method, TraceData

__all__ = [
    # App metadata
    "app_meta",
    "AppMeta",
    "BuildMeta",
    "DeployMeta",
    "EnvironmentMeta",
    "EnvironmentType",
    "CloudProvider",
    "HostedService",
    # Request metadata
    "current_request",
    "RequestMeta",
    "APICallMeta",
    "PubSubMessageMeta",
    "APIDesc",
    "Method",
    "TraceData",
]
