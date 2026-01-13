# Test aliased imports for pub/sub

from dataclasses import dataclass
from encoredev.pubsub import Topic as PubSubTopic

@dataclass
class AliasedMessage:
    value: str

# Should be detected - aliased import from correct module
aliased_topic = PubSubTopic[AliasedMessage]("aliased", {})

# Should NOT be detected - wrong module, even though name is "Topic"
from other.module import Topic as OtherTopic

other = OtherTopic("other")
