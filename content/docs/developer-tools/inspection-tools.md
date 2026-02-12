---
title: "Inspection & Audit Tools"
weight: 3
---
## Inspection & Audit Tools

The `rebar3_nova` plugin includes several commands for inspecting your running application's configuration, middleware chains and security posture. These are useful during development and when debugging production issues.

### View configuration

The `nova config` command displays all Nova configuration values from your sys.config along with their defaults:

```shell
$ rebar3 nova config
=== Nova Configuration ===

  bootstrap_application     my_first_nova
  environment               dev
  cowboy_configuration      #{port => 8080}
  plugins                   [{pre_request,nova_request_plugin,
                              #{decode_json_body => true,
                                read_urlencoded_body => true}}]
  json_lib                  thoas (default)
  use_stacktrace            true
  dispatch_backend          persistent_term (default)
```

This gives you a quick overview of how Nova is configured without having to dig through config files. Keys showing `(default)` are using the built-in default value rather than an explicit setting.

| Key | Default | Description |
|-----|---------|-------------|
| `bootstrap_application` | (required) | Main application to bootstrap |
| `environment` | `dev` | Current environment (`dev`, `prod`, etc.) |
| `cowboy_configuration` | `#{port => 8080}` | Cowboy listener settings |
| `plugins` | `[]` | Global middleware plugins |
| `json_lib` | `thoas` | JSON encoding library |
| `use_stacktrace` | `false` | Include stacktraces in error responses |
| `dispatch_backend` | `persistent_term` | Backend for route dispatch storage |

### Inspect middleware chains

The `nova middleware` command shows the global and per-route-group plugin chains. This is helpful when debugging why a request is being modified or rejected.

```shell
$ rebar3 nova middleware
=== Global Plugins ===
  pre_request: nova_request_plugin #{decode_json_body => true,
                                     read_urlencoded_body => true}

=== Route Groups (my_first_nova_router) ===

  Group: prefix=  security=false
  Plugins:
    (inherits global)
  Routes:
    GET /login -> my_first_nova_main_controller:login
    GET /heartbeat -> (inline fun)
    WS /ws -> my_first_nova_ws_handler

  Group: prefix=  security=fun my_first_nova_auth:username_password/1
  Plugins:
    (inherits global)
  Routes:
    POST / -> my_first_nova_main_controller:index

  Group: prefix=/api  security=false
  Plugins:
    (inherits global)
  Routes:
    GET /users -> my_first_nova_api_controller:index
    GET /users/:id -> my_first_nova_api_controller:show
    POST /users -> my_first_nova_api_controller:create
    GET /products -> my_first_nova_products_controller:list
    GET /products/:id -> my_first_nova_products_controller:show
    POST /products -> my_first_nova_products_controller:create
    PUT /products/:id -> my_first_nova_products_controller:update
    DELETE /products/:id -> my_first_nova_products_controller:delete
```

Each route group shows its prefix, security callback and which plugins apply. Groups that don't set their own plugins inherit the global list.

### Security audit

The `nova audit` command scans your routes and flags potential security issues. It checks for mutation endpoints (POST, PUT, DELETE, PATCH) that don't have a security callback and warns about wildcard method handlers.

```shell
$ rebar3 nova audit
=== Security Audit ===

  WARNINGS:
    POST /api/users (my_first_nova_api_controller) has no security
    POST /api/products (my_first_nova_products_controller) has no security
    PUT /api/products/:id (my_first_nova_products_controller) has no security
    DELETE /api/products/:id (my_first_nova_products_controller) has no security

  INFO:
    GET /login (my_first_nova_main_controller) has no security
    GET /heartbeat has no security
    GET /api/users (my_first_nova_api_controller) has no security
    GET /api/users/:id (my_first_nova_api_controller) has no security
    GET /api/products (my_first_nova_products_controller) has no security
    GET /api/products/:id (my_first_nova_products_controller) has no security

  Summary: 4 warning(s), 6 info(s)
```

The audit classifies findings into two levels:

- **WARNINGS** - mutation methods without security, wildcard method handlers. These likely need attention.
- **INFO** - GET routes without security. These are common for public endpoints but worth reviewing.

This is a good command to run before deploying to make sure you haven't left endpoints unprotected by mistake. For our example app, we intentionally left the API routes open, but in a real application you would add authentication:

```erlang
#{prefix => "/api",
  security => fun my_first_nova_auth:validate_token/1,
  routes => [
    {"/products", fun my_first_nova_products_controller:list/1, #{methods => [get]}},
    {"/products/:id", fun my_first_nova_products_controller:show/1, #{methods => [get]}},
    {"/products", fun my_first_nova_products_controller:create/1, #{methods => [post]}},
    {"/products/:id", fun my_first_nova_products_controller:update/1, #{methods => [put]}},
    {"/products/:id", fun my_first_nova_products_controller:delete/1, #{methods => [delete]}}
  ]}
```

Running the audit again would show no warnings for the `/api` group since all routes now have a security callback.

### Listing routes

We covered `nova routes` earlier in the guide, but it is worth mentioning here alongside the other inspection tools. It displays the compiled routing tree:

```shell
$ rebar3 nova routes
Host: '_'
     ├─  /api
     │   ├─  GET /users (my_first_nova, my_first_nova_api_controller:index/1)
     │   ├─  GET /users/:id (my_first_nova, my_first_nova_api_controller:show/1)
     │   ├─  POST /users (my_first_nova, my_first_nova_api_controller:create/1)
     │   ├─  GET /products (my_first_nova, my_first_nova_products_controller:list/1)
     │   ├─  GET /products/:id (my_first_nova, my_first_nova_products_controller:show/1)
     │   ├─  POST /products (my_first_nova, my_first_nova_products_controller:create/1)
     │   ├─  PUT /products/:id (my_first_nova, my_first_nova_products_controller:update/1)
     │   └─  DELETE /products/:id (my_first_nova, my_first_nova_products_controller:delete/1)
     ├─  GET /login (my_first_nova, my_first_nova_main_controller:login/1)
     ├─  POST / (my_first_nova, my_first_nova_main_controller:index/1)
     ├─  GET /heartbeat
     └─  WS /ws (my_first_nova, my_first_nova_ws_handler)
```

### Summary of inspection commands

| Command | Purpose |
|---------|---------|
| `rebar3 nova config` | Show Nova configuration with defaults |
| `rebar3 nova middleware` | Show global and per-group plugin chains |
| `rebar3 nova audit` | Find routes missing security callbacks |
| `rebar3 nova routes` | Display the compiled routing tree |

These tools together give you full visibility into how your application is wired up. Use `config` to verify settings, `middleware` to trace request processing, `audit` to check security coverage, and `routes` to see the endpoint map.
