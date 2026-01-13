# Protocol type annotations
from typing import Protocol, runtime_checkable

class Printable(Protocol):
    def __str__(self) -> str:
        ...

class Comparable(Protocol):
    def __lt__(self, other: 'Comparable') -> bool:
        ...
    def __eq__(self, other: object) -> bool:
        ...

@runtime_checkable
class Iterable(Protocol[T]):
    def __iter__(self) -> 'Iterator[T]':
        ...

class HasName(Protocol):
    name: str

class HasId(Protocol):
    @property
    def id(self) -> int:
        ...
