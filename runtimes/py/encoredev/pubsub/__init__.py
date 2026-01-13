"""
Pub/Sub module for Encore applications.

This module provides functionality for publishing and subscribing to messages.
"""

from __future__ import annotations

from dataclasses import dataclass
from typing import (
    Any,
    Awaitable,
    Callable,
    Generic,
    Literal,
    NotRequired,
    Optional,
    TypedDict,
    TypeVar,
)

from encoredev.internal.runtime import (
    RT,
    PubSubSubscriptionConfig,
    get_current_request,
    set_current_request,
)

Msg = TypeVar("Msg", bound=object)


# Delivery guarantee types
DeliveryGuarantee = Literal["at-least-once", "exactly-once"]

AT_LEAST_ONCE: DeliveryGuarantee = "at-least-once"
"""
At Least Once delivery guarantees that a message for a subscription is delivered to
a consumer at least once.

On AWS and GCP there is no limit to the throughput for a topic.
"""

EXACTLY_ONCE: DeliveryGuarantee = "exactly-once"
"""
ExactlyOnce guarantees that a message for a subscription is delivered to
a consumer exactly once, to the best of the system's ability.

However, there are edge cases when a message might be redelivered.
It is recommended that the subscription handler function is idempotent
and is able to handle duplicate messages.
"""


class TopicConfig(TypedDict):
    """Configuration for a Pub/Sub topic."""

    delivery_guarantee: DeliveryGuarantee
    """Delivery guarantee for the topic."""

    ordering_attribute: NotRequired[str | None]
    """
    The message attribute to use as an ordering key for messages.
    Delivery will ensure that messages with the same value will
    be delivered in the order they were published.

    If not set, messages can be delivered in any order.
    """


class TopicPerms:
    """Base class for topic permissions."""

    pass


class Publisher(TopicPerms, Generic[Msg]):
    """Publisher interface for a topic."""

    async def publish(self, msg: Msg) -> str:
        """Publish a message to the topic."""
        raise NotImplementedError


class Topic(Publisher[Msg], Generic[Msg]):
    """
    A topic is a resource to which you can publish messages
    to be delivered to subscribers of that topic.

    Example:
        from encoredev.pubsub import Topic, TopicConfig

        @dataclass
        class UserEvent:
            user_id: str
            action: str

        user_events = Topic[UserEvent](
            "user-events",
            TopicConfig(delivery_guarantee="at-least-once")
        )

        # Publish a message
        await user_events.publish(UserEvent(user_id="123", action="login"))
    """

    def __init__(self, name: str, config: TopicConfig) -> None:
        self.name = name
        self.config = config
        self._impl = RT.pubsub_topic(name)

    async def publish(self, msg: Msg) -> str:
        """
        Publish a message to the topic.

        Returns the message ID.
        """
        source = get_current_request()
        return await self._impl.publish(msg, source)

    def ref(self) -> "Topic[Msg]":
        """Return a reference to this topic."""
        return self


@dataclass
class RetryPolicy:
    """
    Defines how a subscription should handle retries
    after errors either delivering or processing the message.
    """

    min_backoff: str | None = None
    """The minimum time to wait between retries. Defaults to 10 seconds."""

    max_backoff: str | None = None
    """The maximum time to wait between retries. Defaults to 10 minutes."""

    max_retries: int | None = None
    """
    MaxRetries is used to control deadletter queuing logic:
    - n == 0: A default value of 100 retries will be used
    - n > 0: Encore will forward a message to a dead letter queue after n retries
    - n == -1 (INFINITE_RETRIES): Messages will not be forwarded to the dead letter queue
    """


INFINITE_RETRIES = -1
"""Value to indicate messages should never be forwarded to the dead letter queue."""


class SubscriptionConfig(TypedDict, Generic[Msg]):
    """Configuration for a Pub/Sub subscription."""

    handler: Callable[[Msg]]
    """
    The function which will be called to process a message sent on the topic.

    When this function raises an error, the message will be negatively acknowledged
    (nacked), which will cause a redelivery attempt to be made (unless the retry
    policy's max_retries has been reached).
    """

    max_concurrency: Optional[int]
    """
    Maximum number of messages which will be processed simultaneously
    per instance of the service for this subscription.

    If the value is negative, then there will be no limit on the number
    of messages processed simultaneously.
    """

    ack_deadline: Optional[str]
    """
    The time a consumer has to process a message before it's returned
    to the subscription. Default is 30 seconds.
    """

    message_retention: Optional[str]
    """
    How long an undelivered message is kept on the topic before it's purged.
    Default is 7 days.
    """

    retry_policy: Optional[RetryPolicy]
    """Defines how a message should be retried when the subscriber returns an error."""


class Subscription(Generic[Msg]):
    """
    A subscription receives messages from a topic.

    Example:
        from encoredev.pubsub import Topic, Subscription, SubscriptionConfig

        @dataclass
        class UserEvent:
            user_id: str
            action: str

        user_events = Topic[UserEvent](
            "user-events",
            TopicConfig(delivery_guarantee="at-least-once")
        )

        async def handle_user_event(msg: UserEvent) -> None:
            print(f"User {msg.user_id} performed {msg.action}")

        user_event_sub = Subscription(
            user_events,
            "user-event-handler",
            SubscriptionConfig(handler=handle_user_event)
        )
    """

    def __init__(
        self,
        topic: Topic[Msg],
        name: str,
        config: SubscriptionConfig[Msg],
    ) -> None:
        self._topic = topic
        self._name = name
        self._config = config

        async def handler_wrapper(msg: Any):
            set_current_request(msg)
            payload = msg.payload()
            result = config["handler"](payload)
            # Await if the handler is async
            if hasattr(result, "__await__"):
                await result

        self._impl = RT.pubsub_subscription(
            PubSubSubscriptionConfig(
                topic_name=topic.name,
                subscription_name=name,
                handler=handler_wrapper,
            )
        )


# Type for marking message attributes
Attribute = TypeVar("Attribute", str, int, bool)
"""
Type marker for fields that should be sent as attributes in a PubSub message,
rather than in the message body.

This is useful for ordering messages, or for filtering messages
on a subscription - otherwise you should not use this.
"""


__all__ = [
    "AT_LEAST_ONCE",
    "Attribute",
    "DeliveryGuarantee",
    "EXACTLY_ONCE",
    "INFINITE_RETRIES",
    "Publisher",
    "RetryPolicy",
    "Subscription",
    "SubscriptionConfig",
    "Topic",
    "TopicConfig",
    "TopicPerms",
]
