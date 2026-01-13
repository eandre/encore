# Function signature type annotations

def simple_function(x: int, y: str) -> bool:
    pass

def with_optional(name: str, age: int = 0) -> None:
    pass

def with_args(*args: int) -> list[int]:
    pass

def with_kwargs(**kwargs: str) -> dict[str, str]:
    pass

def mixed_params(a: int, b: str, *args: float, c: bool = True, **kwargs: int) -> None:
    pass

def positional_only(x: int, y: str, /) -> bool:
    pass

def keyword_only(*, name: str, age: int) -> None:
    pass

async def async_function(url: str) -> bytes:
    pass

def generic_function(items: list[T]) -> T:
    pass
