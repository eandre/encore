# Control flow statements and scoping
# Note: if, for, while, try, with do NOT create new scopes

# For loop - loop variable is in enclosing scope
for i in range(10):
    loop_var = i * 2

# i and loop_var are still accessible here
print(i, loop_var)

# Nested for loops
for x in range(3):
    for y in range(3):
        cell = (x, y)

# x, y, cell all accessible here

# While loop
count = 0
while count < 5:
    while_var = count
    count += 1

# while_var accessible here

# If statement
condition = True
if condition:
    if_var = "true branch"
else:
    if_var = "false branch"

# if_var accessible here

# With statement - binds in enclosing scope
with open("/dev/null") as f:
    content = f.read()

# f and content accessible here (though f is closed)

# Multiple with items
with open("/dev/null") as f1, open("/dev/null") as f2:
    data1 = f1.read()
    data2 = f2.read()

# Try/except - exception variable has special scoping
try:
    risky_operation = 1 / 0
except ZeroDivisionError as e:
    error_msg = str(e)
    # e is bound here

# error_msg accessible, but e is deleted after except block in Python 3
# risky_operation may or may not be bound depending on where exception occurred

# Try/except/else/finally
try:
    result = "success"
except Exception as ex:
    result = "error"
else:
    else_var = "no exception"
finally:
    finally_var = "cleanup"

# All these are accessible here (except ex which is deleted)


def function_with_control_flow():
    # All bindings here are local to function
    for item in [1, 2, 3]:
        local_loop_var = item

    if True:
        local_if_var = "set"

    try:
        local_try_var = "attempted"
    except Exception as local_ex:
        local_except_var = "caught"

    # All local_* variables accessible here
    return local_loop_var
