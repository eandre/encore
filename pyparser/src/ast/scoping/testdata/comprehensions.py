# Comprehension scoping: Python 3 creates new scope for comprehensions

items = [1, 2, 3, 4, 5]

# List comprehension - x is scoped to comprehension
squares = [x * x for x in items]

# x here is a new global binding, not the comprehension's x
x = "global x"

# Nested comprehension
matrix = [[1, 2], [3, 4], [5, 6]]
flattened = [cell for row in matrix for cell in row]

# Set comprehension
unique = {x * 2 for x in items}

# Dict comprehension
mapping = {k: v for k, v in enumerate(items)}

# Generator expression
gen = (x ** 2 for x in range(10))

# Comprehension with condition
evens = [x for x in items if x % 2 == 0]

# Nested comprehensions with same variable name
result = [[y for y in range(x)] for x in range(3)]


def with_comprehension():
    local_items = [1, 2, 3]
    # i is local to comprehension, not to function
    doubled = [i * 2 for i in local_items]
    # i is not defined here
    return doubled
