# Import statement scoping

# Simple import
import os
import sys

# Import with alias
import json as j
import collections.abc as abc_module

# From import
from typing import List, Dict, Optional
from dataclasses import dataclass, field

# From import with alias
from pathlib import Path as P
from functools import reduce as fold

# Relative imports (these would be resolved based on package context)
# from . import sibling
# from .submodule import something
# from ..parent import other

# Multiple imports on one line
import math, random, time

# Nested usage
def use_imports():
    # These access the module-level imports
    result = os.getcwd()
    data = j.dumps({"key": "value"})
    items: List[int] = [1, 2, 3]
    return result
