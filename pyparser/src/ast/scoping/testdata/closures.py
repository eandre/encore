# Closures and nested functions

def make_counter(start=0):
    count = start

    def increment():
        nonlocal count
        count += 1
        return count

    def decrement():
        nonlocal count
        count -= 1
        return count

    def get():
        return count

    return increment, decrement, get


def make_multiplier(factor):
    # factor is captured from enclosing scope
    def multiply(x):
        return x * factor

    return multiply


def deep_nesting():
    a = 1

    def level1():
        b = 2

        def level2():
            c = 3

            def level3():
                d = 4
                # Can access a, b, c from enclosing scopes
                return a + b + c + d

            return level3

        return level2

    return level1


def closure_with_loop():
    funcs = []
    for i in range(3):
        # Common pitfall: i is shared
        def f():
            return i

        funcs.append(f)
    return funcs


def closure_with_loop_fixed():
    funcs = []
    for i in range(3):
        # Fixed: capture i as default argument
        def f(x=i):
            return x

        funcs.append(f)
    return funcs
