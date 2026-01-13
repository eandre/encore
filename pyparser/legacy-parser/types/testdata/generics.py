# Generic type annotations
from typing import TypeVar, Generic

T = TypeVar('T')
K = TypeVar('K')
V = TypeVar('V')

# Bounded TypeVar
Numeric = TypeVar('Numeric', int, float)
Comparable = TypeVar('Comparable', bound=str)

# Covariant/Contravariant
T_co = TypeVar('T_co', covariant=True)
T_contra = TypeVar('T_contra', contravariant=True)

# Generic class usage
class Container(Generic[T]):
    value: T

class Mapping(Generic[K, V]):
    key: K
    value: V
