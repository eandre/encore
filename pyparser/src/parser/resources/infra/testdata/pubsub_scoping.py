# Test scoping scenarios for pub/sub topic detection

from dataclasses import dataclass
from encoredev.pubsub import Topic

@dataclass
class ScopedMessage:
    content: str

# This should be detected - module level topic
module_topic = Topic[ScopedMessage]("module-topic", {})


def some_function():
    # This should NOT be detected - Topic is shadowed
    Topic = str
    not_a_topic = Topic("just-a-string")


class MyService:
    # This should be detected - class level topic
    class_topic = Topic[ScopedMessage]("class-topic", {})

    def method(self):
        # This should be detected - uses outer import
        method_topic = Topic[ScopedMessage]("method-topic", {})


# After the function, Topic is still the real one
after_shadow = Topic[ScopedMessage]("after-shadow", {})


def nested_function():
    def inner():
        # Should be detected - no shadowing in this scope
        inner_topic = Topic[ScopedMessage]("inner-topic", {})
    inner()
