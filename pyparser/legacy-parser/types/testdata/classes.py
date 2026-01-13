# Class type annotations
from dataclasses import dataclass
from typing import ClassVar, Final

class SimpleClass:
    name: str
    age: int

class WithDefaults:
    name: str = "default"
    count: int = 0

class WithClassVar:
    instances: ClassVar[int] = 0
    name: str

class WithFinal:
    MAX_SIZE: Final[int] = 100
    value: int

@dataclass
class DataClass:
    id: int
    name: str
    active: bool = True

class Inheriting(SimpleClass):
    email: str

class MultipleInheritance(SimpleClass, WithDefaults):
    role: str
