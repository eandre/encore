# TypedDict type annotations
from typing import TypedDict, Required, NotRequired

class UserDict(TypedDict):
    name: str
    age: int
    email: str

class PartialDict(TypedDict, total=False):
    name: str
    nickname: str

class MixedDict(TypedDict):
    id: Required[int]
    name: str
    metadata: NotRequired[dict[str, str]]

# Inline TypedDict
Point = TypedDict('Point', {'x': int, 'y': int})
