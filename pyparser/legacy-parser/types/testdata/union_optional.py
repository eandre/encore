# Union and Optional type annotations
from typing import Union, Optional

# Optional types
maybe_name: Optional[str]
maybe_age: Optional[int]

# Union types
string_or_int: Union[str, int]
multi_union: Union[str, int, float, None]

# PEP 604 union syntax (Python 3.10+)
pep604_union: str | int
pep604_optional: str | None
pep604_multi: str | int | float | None
