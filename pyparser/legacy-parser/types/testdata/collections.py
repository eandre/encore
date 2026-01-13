# Collection type annotations
from typing import List, Dict, Set, FrozenSet, Tuple

# List types
names: list[str]
numbers: List[int]
nested_list: list[list[str]]

# Dict types
mapping: dict[str, int]
complex_dict: Dict[str, list[int]]

# Set types
unique_ids: set[int]
frozen_ids: frozenset[str]
typed_set: Set[float]
typed_frozenset: FrozenSet[int]

# Tuple types
point: tuple[int, int]
mixed_tuple: Tuple[str, int, bool]
variable_tuple: tuple[int, ...]
