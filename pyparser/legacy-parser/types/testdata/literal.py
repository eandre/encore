# Literal type annotations
from typing import Literal

# String literals
direction: Literal["north", "south", "east", "west"]
status: Literal["pending", "active", "completed"]

# Numeric literals
dice_roll: Literal[1, 2, 3, 4, 5, 6]
bool_literal: Literal[True, False]

# Mixed literals
mixed: Literal["auto", 0, 1, True]
