# Test that Topic from wrong module is not detected

from other.module import Topic

# Should NOT be detected - wrong module
not_encore = Topic("not-from-encore")


class MyTopic:
    """A user-defined Topic class - should not be detected."""
    def __init__(self, name):
        self.name = name


# Using user-defined class - should not be detected
Topic = MyTopic
user_defined = Topic("user-defined")
