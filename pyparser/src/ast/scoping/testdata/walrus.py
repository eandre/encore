# Named expressions (walrus operator :=)
# The walrus operator binds in the enclosing non-comprehension scope

# Basic usage
if (n := 10) > 5:
    print(n)  # n is bound in module scope

# In while loop
data = [1, 2, 3, 4, 5]
index = 0
while (value := data[index] if index < len(data) else None) is not None:
    print(value)
    index += 1

# In list comprehension - binds to enclosing scope, not comprehension
# filtered is bound in module scope
results = [y for x in range(10) if (y := x * 2) > 5]
# y is accessible here because it's bound in module scope

# In nested comprehension
outer_result = [
    inner
    for outer in range(3)
    if (inner := [x for x in range(outer)])
]

# In function
def process_data():
    items = [1, 2, 3, 4, 5]
    # last is bound in function scope, not comprehension
    processed = [last := x * 2 for x in items]
    return last  # Returns the last value (10)


# Multiple walrus in same scope
def multi_walrus():
    if (a := 1) and (b := 2) and (c := 3):
        return a + b + c


# Walrus in conditional expression
def conditional_walrus(data):
    return (result := data.get("key")) if result else "default"


# Walrus with any/all
numbers = [1, 2, 3, 4, 5]
has_even = any((matched := n) % 2 == 0 for n in numbers)
# matched is bound in module scope
