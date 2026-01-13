# Basic scoping: local and global variables

# Global variable
x = 1
y = "hello"

def foo():
    # Local variable shadows global
    x = 2
    z = 3
    return x + z

def bar():
    # Access global x (no local binding)
    return x + 1

# Multiple assignment
a, b, c = 1, 2, 3

# Annotated assignment
typed_var: int = 42
