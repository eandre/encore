# Callable type annotations
from typing import Callable

# Simple callable
simple_fn: Callable[[], None]
with_args: Callable[[int, str], bool]
any_args: Callable[..., int]

# Nested callable
higher_order: Callable[[Callable[[int], int]], int]
returns_callable: Callable[[int], Callable[[str], bool]]
