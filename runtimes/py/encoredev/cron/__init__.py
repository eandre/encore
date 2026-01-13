"""
Cron job module for Encore applications.

This module provides functionality for defining scheduled cron jobs.
"""

from __future__ import annotations

from dataclasses import dataclass
from typing import Awaitable, Callable, Literal, Union


@dataclass
class CronJobConfig:
    """Configuration for a cron job."""

    endpoint: Callable[[], Awaitable[object]]
    """The endpoint function to call when the cron job runs."""

    title: str | None = None
    """Optional title for the cron job."""

    every: str | None = None
    """
    Duration string for how often to run the job.
    Mutually exclusive with `schedule`.
    Examples: "1h", "30m", "1h30m"
    """

    schedule: str | None = None
    """
    Cron expression for when to run the job.
    Mutually exclusive with `every`.
    Example: "0 0 * * *" (daily at midnight)
    """

    def __post_init__(self) -> None:
        if self.every is None and self.schedule is None:
            raise ValueError("Either 'every' or 'schedule' must be specified")
        if self.every is not None and self.schedule is not None:
            raise ValueError("Only one of 'every' or 'schedule' can be specified")


class CronJob:
    """
    Defines a cron job that runs on a schedule.

    Cron jobs are automatically executed by the Encore runtime
    according to the specified schedule.

    Example:
        from encoredev.cron import CronJob, CronJobConfig

        daily_cleanup = CronJob(
            "daily-cleanup",
            CronJobConfig(
                endpoint=cleanup_endpoint,
                title="Daily Cleanup",
                schedule="0 0 * * *",
            )
        )

        hourly_sync = CronJob(
            "hourly-sync",
            CronJobConfig(
                endpoint=sync_endpoint,
                title="Hourly Sync",
                every="1h",
            )
        )
    """

    def __init__(self, name: str, config: CronJobConfig) -> None:
        self.name = name
        self.config = config


__all__ = ["CronJob", "CronJobConfig"]
