# Annotated type annotations (pydantic-like)
from typing import Annotated

# Simple annotated
name: Annotated[str, "user name"]
age: Annotated[int, "user age in years"]

# With validation constraints (pydantic style)
username: Annotated[str, Field(min_length=3, max_length=50)]
score: Annotated[int, Field(ge=0, le=100)]
email: Annotated[str, Field(pattern=r'^[\w\.-]+@[\w\.-]+\.\w+$')]

# Multiple annotations
validated_name: Annotated[str, Field(min_length=1), "required field"]

# Numeric constraints
positive_int: Annotated[int, Field(gt=0)]
percentage: Annotated[float, Field(ge=0.0, le=1.0)]
