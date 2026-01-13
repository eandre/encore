"""
SQL database module for Encore applications.

This module provides functionality for SQL database operations.
"""

from __future__ import annotations

from typing import Any, AsyncGenerator, TypedDict, TypeVar

from encoredev.internal import RT
from encoredev.internal.runtime import (
    QueryArgs,
    get_current_request,
)

# Type aliases
Row = dict[str, Any]
"""Represents a single row from a query result."""

Primitive = (
    str
    | list[str]
    | int
    | list[int]
    | float
    | list[float]
    | bool
    | list[bool]
    | bytes
    | None
)
"""Represents a type that can be used in query parameters."""

T = TypeVar("T", bound=Row)


class SQLDatabaseConfig(TypedDict):
    """Configuration for a Pub/Sub topic."""

    """The path to where the database migrations directory is located."""
    migrations: str | None


class BaseQueryExecutor:
    """Base class containing shared query functionality."""

    def __init__(self, impl: Any) -> None:
        self._impl = impl

    async def query(
        self,
        sql: str,
        *params: Primitive,
    ) -> AsyncGenerator[Row, None]:
        """
        Execute a query and return results as an async generator.

        Example:
            async for row in db.query("SELECT * FROM users WHERE id = $1", user_id):
                print(row)
        """
        args = QueryArgs(list(params))
        source = get_current_request()
        cursor = await self._impl.query(sql, args, source)
        while True:
            row = await cursor.next()
            if row is None:
                break
            yield row.values()

    async def query_all(
        self,
        sql: str,
        *params: Primitive,
    ) -> list[Row]:
        """
        Execute a query and return all results as a list.

        Example:
            rows = await db.query_all("SELECT * FROM users WHERE active = $1", True)
        """
        result: list[Row] = []
        async for row in self.query(sql, *params):
            result.append(row)
        return result

    async def query_row(
        self,
        sql: str,
        *params: Primitive,
    ) -> Row | None:
        """
        Execute a query and return only the first row.

        Returns None if no rows are returned.

        Example:
            row = await db.query_row("SELECT * FROM users WHERE id = $1", user_id)
        """

        args = QueryArgs(list(params))
        source = get_current_request()
        row = await self._impl.query_row(sql, args, source)
        if row is None:
            return None
        return row.values()

    async def exec(
        self,
        sql: str,
        *params: Primitive,
    ) -> None:
        """
        Execute a query without returning any rows.

        Example:
            await db.exec("DELETE FROM users WHERE id = $1", user_id)
        """
        args = QueryArgs(list(params))
        source = get_current_request()
        cursor = await self._impl.query(sql, args, source)
        await cursor.next()


class SQLDatabase(BaseQueryExecutor):
    """
    SQL database resource.

    Constructing a new SQLDatabase object will result in Encore provisioning
    a database with that name and returning this object to represent it.

    Example:
        from encoredev.storage.sqldb import SQLDatabase, SQLDatabaseConfig

        db = SQLDatabase(
            "mydb",
            SQLDatabaseConfig(migrations="./migrations")
        )

        # Query the database
        async for row in db.query("SELECT * FROM users"):
            print(row)

        # Execute a statement
        await db.exec("INSERT INTO users (name) VALUES ($1)", "John")
    """

    def __init__(self, name: str, config: SQLDatabaseConfig | None = None) -> None:
        impl = RT.sql_database(name)
        super().__init__(impl)
        self._name = name
        self._config = config

    @classmethod
    def named(cls, name: str) -> "SQLDatabase":
        """
        Reference an existing database by name.

        If the database doesn't exist yet, use `SQLDatabase(name)` instead.
        """
        return cls(name)

    @property
    def connection_string(self) -> str:
        """Returns the connection string for the database."""
        return self._impl.conn_string()

    async def acquire(self) -> "Connection":
        """
        Acquire a database connection from the pool.

        When the connection is closed or garbage-collected, it is returned to the pool.
        """
        impl = await self._impl.acquire()
        return Connection(impl)

    async def begin(self) -> "Transaction":
        """
        Begin a database transaction.

        Make sure to always call `rollback()` or `commit()` to prevent hanging transactions.

        Can be used as an async context manager:
            async with await db.begin() as tx:
                await tx.exec("INSERT INTO users (name) VALUES ($1)", "John")
                # Automatically rolls back if an exception is raised
        """
        source = get_current_request()
        impl = await self._impl.begin(source)
        return Transaction(impl)


class Connection(BaseQueryExecutor):
    """
    Represents a dedicated connection to a database.
    """

    async def close(self) -> None:
        """Returns the connection to the database pool."""
        await self._impl.close()

    async def __aenter__(self) -> "Connection":
        return self

    async def __aexit__(self, exc_type: Any, exc_val: Any, exc_tb: Any) -> None:
        await self.close()


class Transaction(BaseQueryExecutor):
    """
    Represents a database transaction.
    """

    def __init__(self, impl: Any) -> None:
        super().__init__(impl)
        self._done = False

    async def commit(self) -> None:
        """Commit the transaction."""
        self._done = True
        source = get_current_request()
        await self._impl.commit(source)

    async def rollback(self) -> None:
        """Rollback the transaction."""
        self._done = True
        source = get_current_request()
        await self._impl.rollback(source)

    async def __aenter__(self) -> "Transaction":
        return self

    async def __aexit__(self, exc_type: Any, exc_val: Any, exc_tb: Any) -> None:
        if not self._done:
            await self.rollback()


__all__ = [
    "Connection",
    "Primitive",
    "Row",
    "SQLDatabase",
    "SQLDatabaseConfig",
    "SQLMigrationsConfig",
    "Transaction",
]
