# Basic metrics definitions

from encoredev.metrics import Counter, CounterGroup, Gauge, GaugeGroup

# Simple counter
requests_total = Counter("requests_total")

# Counter group with labels
http_requests = CounterGroup[dict]("http_requests_total")

# Simple gauge
active_connections = Gauge("active_connections")

# Gauge group with labels
cpu_usage = GaugeGroup[dict]("cpu_usage_percent")
