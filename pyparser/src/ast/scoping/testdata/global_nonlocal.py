# Global and nonlocal declarations

counter = 0

def increment():
    global counter
    counter += 1
    return counter


def outer():
    x = 10

    def inner():
        nonlocal x
        x += 1
        return x

    return inner


def multi_level():
    a = 1

    def level1():
        b = 2

        def level2():
            nonlocal b
            b += 10

            def level3():
                nonlocal b
                b += 100
                return b

            return level3

        return level2

    return level1


def mixed_declarations():
    global counter

    local_var = "local"

    def nested():
        nonlocal local_var
        global counter
        local_var = "modified"
        counter += 1

    nested()
    return local_var
