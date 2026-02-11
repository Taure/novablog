---
title: "OpenTelemetry: Traces and Metrics"
weight: 5
---
## OpenTelemetry: Traces and Metrics

When your Nova application is running in production you need visibility into what it is doing. [OpenTelemetry](https://opentelemetry.io/) is the industry standard for collecting traces and metrics from your services. The [`opentelemetry_nova`](https://github.com/novaframework/opentelemetry_nova) library gives you automatic instrumentation — every HTTP request gets a trace span and metrics are recorded without writing any manual instrumentation code.

In this article we will set up `opentelemetry_nova` to collect traces and Prometheus metrics from a Nova application.

### What you get

Once configured, `opentelemetry_nova` automatically provides:

**Distributed traces** — Every incoming HTTP request creates a span with attributes like method, path, status code, controller, and action. If the caller sends a W3C `traceparent` header, the span is linked to the upstream trace automatically.

**HTTP metrics** — Four metrics are recorded for every request:

| Metric | Type | Description |
|---|---|---|
| `http.server.request.duration` | Histogram | Request duration in seconds |
| `http.server.active_requests` | Gauge | Number of in-flight requests |
| `http.server.request.body.size` | Histogram | Request body size in bytes |
| `http.server.response.body.size` | Histogram | Response body size in bytes |

### Adding the dependency

Add `opentelemetry_nova` and the OpenTelemetry SDK to your `rebar.config`:

```erlang
{deps, [
    nova,
    {opentelemetry, "~> 1.5"},
    {opentelemetry_experimental, "~> 0.5"},
    {opentelemetry_exporter, "~> 1.8"},
    opentelemetry_nova
]}.
```

### Configuring the stream handler

`opentelemetry_nova` uses a Cowboy stream handler to intercept requests. Add `otel_nova_stream_h` to the Nova cowboy configuration in your `sys.config`:

```erlang
{nova, [
    {cowboy_configuration, #{
        port => 8080,
        stream_handlers => [otel_nova_stream_h, cowboy_stream_h]
    }}
]}
```

The order matters — `otel_nova_stream_h` must come **before** `cowboy_stream_h` so it can wrap the full request lifecycle.

### Setting up tracing

Configure the OpenTelemetry SDK to export traces. Here we send them via OTLP HTTP to a collector running on localhost:

```erlang
{opentelemetry, [
    {span_processor, batch},
    {traces_exporter, {opentelemetry_exporter, #{
        protocol => http_protobuf,
        endpoints => [#{host => "localhost", port => 4318, path => "/v1/traces"}]
    }}}
]},

{opentelemetry_exporter, [
    {otlp_protocol, http_protobuf},
    {otlp_endpoint, "http://localhost:4318"}
]}
```

This sends traces to any OTLP-compatible backend — Grafana Tempo, Jaeger, or any OpenTelemetry Collector.

### Setting up Prometheus metrics

Configure a metric reader that uses the built-in Prometheus exporter:

```erlang
{opentelemetry_experimental, [
    {readers, [
        #{module => otel_metric_reader,
          config => #{
              export_interval_ms => 5000,
              exporter => {otel_nova_prom_exporter, #{}}
          }}
    ]}
]}
```

Then in your application's `start/2` function, call `opentelemetry_nova:setup/1` to initialize the metrics and start the Prometheus HTTP server:

```erlang
-module(my_app_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    opentelemetry_nova:setup(#{prometheus => #{port => 9464}}),
    my_app_sup:start_link().

stop(_State) ->
    ok.
```

This starts a Prometheus metrics endpoint at `http://localhost:9464/metrics`. Point your Prometheus server or Grafana Agent at this endpoint to scrape the metrics.

If you only want metrics without the Prometheus HTTP server (for example if you are pushing metrics via OTLP instead), call `opentelemetry_nova:setup()` with no arguments.

### Adding the Nova plugin for span enrichment

The stream handler creates spans with basic HTTP attributes. To also get the Nova controller and action on each span, add the `otel_nova_plugin` as a pre-request plugin on your routes:

```erlang
routes(_Environment) ->
    [#{
        plugins => [{pre_request, otel_nova_plugin, #{}}],
        routes => [
            {"/hello", fun my_controller:hello/1, #{methods => [get]}},
            {"/users", fun my_controller:users/1, #{methods => [get, post]}}
        ]
    }].
```

With this plugin, spans get enriched with `nova.app`, `nova.controller`, and `nova.action` attributes, and the span name is updated to include the controller — for example `GET my_controller:hello` instead of just `HTTP GET`.

### Full sys.config example

Here is a complete `sys.config` bringing everything together:

```erlang
[
  {nova, [
      {cowboy_configuration, #{
          port => 8080,
          stream_handlers => [otel_nova_stream_h, cowboy_stream_h]
      }}
  ]},

  {opentelemetry, [
      {span_processor, batch},
      {traces_exporter, {opentelemetry_exporter, #{
          protocol => http_protobuf,
          endpoints => [#{host => "localhost", port => 4318, path => "/v1/traces"}]
      }}}
  ]},

  {opentelemetry_experimental, [
      {readers, [
          #{module => otel_metric_reader,
            config => #{
                export_interval_ms => 5000,
                exporter => {otel_nova_prom_exporter, #{}}
            }}
      ]}
  ]},

  {opentelemetry_exporter, [
      {otlp_protocol, http_protobuf},
      {otlp_endpoint, "http://localhost:4318"}
  ]}
].
```

### Verifying it works

Start your application and make a few requests:

```shell
curl http://localhost:8080/hello
curl -X POST -d '{"name":"nova"}' http://localhost:8080/users
```

Then check the Prometheus metrics endpoint:

```shell
curl http://localhost:9464/metrics
```

You should see output like:

```
# HELP http_server_request_duration_seconds Duration of HTTP server requests
# TYPE http_server_request_duration_seconds histogram
http_server_request_duration_seconds_bucket{method="GET",scheme="http",server_address="localhost",server_port="8080",response_status_code="200",le="0.005"} 1
...
http_server_request_duration_seconds_count{method="GET",scheme="http",server_address="localhost",server_port="8080",response_status_code="200"} 1
http_server_request_duration_seconds_sum{method="GET",scheme="http",server_address="localhost",server_port="8080",response_status_code="200"} 0.002

# HELP http_server_active_requests Number of active HTTP server requests
# TYPE http_server_active_requests gauge
http_server_active_requests{method="GET",scheme="http",server_address="localhost",server_port="8080"} 0
```

For traces, check your configured backend (Tempo, Jaeger, etc.) and you should see spans for each request with the full set of HTTP and Nova attributes.

### How it works under the hood

The `otel_nova_stream_h` stream handler sits in Cowboy's stream pipeline. When a request arrives it:

1. Extracts any incoming trace context from the `traceparent` header
2. Creates a new server span named `HTTP <method>`
3. Sets request attributes (method, path, scheme, host, port, peer address, user agent)
4. Increments the active requests counter

As the request flows through to the controller and back, the handler captures the response status code and body sizes.

When the request terminates it:

1. Sets the response status code attribute
2. Marks the span as error if the status is 500 or above
3. Ends the span
4. Records the request duration, request body size, and response body size metrics
5. Decrements the active requests counter

The `otel_nova_plugin` runs as a normal Nova pre-request plugin. It extracts the controller module and function from the route match and sets them as span attributes.

### Running with a full observability stack

The [nova_otel_demo](https://github.com/novaframework/nova_otel_demo) repository has a complete example with a Docker Compose setup that includes:

- **OpenTelemetry Collector** — receives traces and metrics via OTLP
- **Grafana Tempo** — stores and queries traces
- **Grafana Mimir** — stores Prometheus metrics
- **Grafana** — dashboards and trace exploration

Clone it and run `docker-compose up` from the `docker/` directory to get a full observability stack connected to a Nova application.
