# Encore SDK for Python

The Encore SDK for Python provides a type-safe, idiomatic way to build backend applications with [Encore](https://encore.dev).

## Installation

```bash
pip install encoredev
```

## Quick Start

### Defining an API Endpoint

```python
from dataclasses import dataclass
from encoredev.api import api

@dataclass
class HelloResponse:
    message: str

@api(method="GET", path="/hello/:name")
async def hello(name: str) -> HelloResponse:
    return HelloResponse(message=f"Hello, {name}!")
```

### Defining a Service

```python
from encoredev.service import Service

svc = Service("my-service")
```

## Modules

### `encoredev` - Core

Access application and request metadata:

```python
from encoredev import app_meta, current_request

# Get application metadata
meta = app_meta()
print(meta.environment.name)

# Get current request info
req = current_request()
if req and req.type == "api-call":
    print(req.path)
```

### `encoredev.api` - API Endpoints

Define HTTP API endpoints with type-safe request/response handling:

```python
from dataclasses import dataclass
from encoredev.api import api, APIError, ErrCode

@dataclass
class User:
    id: int
    name: str

@api(method="GET", path="/users/:id", expose=True)
async def get_user(id: int) -> User:
    user = await fetch_user(id)
    if not user:
        raise APIError.not_found(f"User {id} not found")
    return user
```

### `encoredev.auth` - Authentication

Define authentication handlers:

```python
from dataclasses import dataclass
from encoredev.auth import auth_handler

@dataclass
class AuthParams:
    token: str

@dataclass
class UserData:
    user_id: str
    email: str

@auth_handler
async def my_auth(params: AuthParams) -> UserData | None:
    user = await validate_token(params.token)
    if user:
        return UserData(user_id=user.id, email=user.email)
    return None
```

### `encoredev.config` - Configuration & Secrets

Load secrets securely:

```python
from encoredev.config import secret

api_key = secret("api_key")

# Use the secret
key = api_key()
```

### `encoredev.cron` - Scheduled Jobs

Define cron jobs:

```python
from encoredev.cron import CronJob, CronJobConfig

daily_cleanup = CronJob(
    "daily-cleanup",
    CronJobConfig(
        endpoint=cleanup_endpoint,
        schedule="0 0 * * *",  # Daily at midnight
    )
)

hourly_sync = CronJob(
    "hourly-sync",
    CronJobConfig(
        endpoint=sync_endpoint,
        every="1h",  # Every hour
    )
)
```

### `encoredev.log` - Structured Logging

Log messages with structured fields:

```python
from encoredev.log import info, error, Logger

# Simple logging
info("Processing request", {"user_id": "123"})

# With errors
try:
    do_something()
except Exception as e:
    error(e, "Operation failed", {"context": "important"})

# Custom logger with fields
logger = Logger().with_fields({"service": "users"})
logger.info("User created", {"user_id": "456"})
```

### `encoredev.metrics` - Observability

Define custom metrics:

```python
from encoredev.metrics import Counter, CounterGroup, Gauge
from typing import TypedDict

# Simple counter
requests = Counter("http_requests")
requests.increment()

# Counter with labels
class Labels(TypedDict):
    method: str
    status: int

requests_by_status = CounterGroup[Labels]("http_requests_by_status")
requests_by_status.with_({"method": "GET", "status": 200}).increment()

# Gauge for current values
connections = Gauge("active_connections")
connections.set(42)
```

### `encoredev.pubsub` - Pub/Sub Messaging

Publish and subscribe to messages:

```python
from dataclasses import dataclass
from encoredev.pubsub import Topic, Subscription, TopicConfig, SubscriptionConfig

@dataclass
class UserEvent:
    user_id: str
    action: str

# Define a topic
user_events = Topic[UserEvent](
    "user-events",
    TopicConfig(delivery_guarantee="at-least-once")
)

# Publish messages
await user_events.publish(UserEvent(user_id="123", action="login"))

# Subscribe to messages
async def handle_event(msg: UserEvent) -> None:
    print(f"User {msg.user_id} performed {msg.action}")

user_sub = Subscription(
    user_events,
    "user-handler",
    SubscriptionConfig(handler=handle_event)
)
```

### `encoredev.storage.sqldb` - SQL Databases

Work with SQL databases:

```python
from encoredev.storage.sqldb import SQLDatabase, SQLDatabaseConfig

db = SQLDatabase("mydb", SQLDatabaseConfig(migrations="./migrations"))

# Query rows
async for row in db.query("SELECT * FROM users WHERE active = $1", True):
    print(row)

# Get single row
user = await db.query_row("SELECT * FROM users WHERE id = $1", user_id)

# Execute statements
await db.exec("UPDATE users SET last_login = NOW() WHERE id = $1", user_id)

# Transactions
async with await db.begin() as tx:
    await tx.exec("INSERT INTO orders (user_id) VALUES ($1)", user_id)
    await tx.exec("UPDATE inventory SET count = count - 1 WHERE id = $1", item_id)
    await tx.commit()
```

### `encoredev.storage.objects` - Object Storage

Work with object storage buckets:

```python
from encoredev.storage.objects import Bucket, BucketConfig, UploadOptions, ListOptions

images = Bucket("images", BucketConfig(public=True))

# Upload an object
await images.upload(
    "photo.jpg",
    image_data,
    UploadOptions(content_type="image/jpeg")
)

# Download an object
data = await images.download("photo.jpg")

# List objects
async for entry in images.list(ListOptions(prefix="photos/")):
    print(entry.name, entry.size)

# Get public URL
url = images.public_url("photo.jpg")
```

### `encoredev.types` - Custom Types

Use arbitrary precision decimals:

```python
from encoredev.types import Decimal

price = Decimal("19.99")
quantity = Decimal(3)
total = price * quantity  # Decimal("59.97")
```

### `encoredev.validate` - Validation

Add validation constraints to fields:

```python
from dataclasses import dataclass
from typing import Annotated
from encoredev.validate import Min, Max, MinLen, MaxLen, IsEmail

@dataclass
class CreateUser:
    name: Annotated[str, MinLen(1), MaxLen(100)]
    email: Annotated[str, IsEmail]
    age: Annotated[int, Min(0), Max(150)]
```

## Middleware

Define middleware for cross-cutting concerns:

```python
from encoredev.api import middleware, MiddlewareRequest, HandlerResponse, Next

@middleware
async def logging_middleware(req: MiddlewareRequest, next: Next) -> HandlerResponse:
    print(f"Request: {req.request_meta}")
    response = await next(req)
    print(f"Response: {response.payload}")
    return response
```

## Documentation

For full documentation, visit [encore.dev/docs](https://encore.dev/docs).

## License

This project is licensed under the Mozilla Public License 2.0 - see the LICENSE file for details.
