---
title: "CORS - Cross-Origin Resource Sharing"
date: 2024-10-28T00:00:00+00:00
weight: 18
tags: ["nova", "erlang", "cors", "api"]
series: ["Nova Framework Guide"]
summary: "Configuring CORS for API consumption from different domains."
---

## CORS - Cross-Origin Resource Sharing

If you are building an API that is consumed by a frontend running on a different domain, the browser will block the requests unless your server sends the right CORS headers. Nova includes a CORS plugin that handles this for you.

### What is CORS?

When a browser makes a request from `https://myapp.com` to your API at `https://api.myapp.com`, it is a cross-origin request. Browsers block these by default for security. To allow them, your server needs to respond with headers like `Access-Control-Allow-Origin` telling the browser which origins are permitted.

For some requests (like POST with JSON body), the browser first sends a preflight `OPTIONS` request to check if the actual request is allowed.

### Using nova_cors_plugin

Nova ships with `nova_cors_plugin`. Add it to your plugin configuration in `sys.config`:

```erlang
{plugins, [
    {pre_request, nova_cors_plugin, #{allow_origins => <<"*">>}},
    {pre_request, nova_request_plugin, #{decode_json_body => true}}
]}
```

The `allow_origins` option sets the `Access-Control-Allow-Origin` header. Using `<<"*">>` allows requests from any origin. For production you should restrict this to your frontend's domain:

```erlang
{pre_request, nova_cors_plugin, #{allow_origins => <<"https://myapp.com">>}}
```

### What the plugin does

The CORS plugin does two things:

1. **Adds CORS headers** to every response:
   - `Access-Control-Allow-Origin` — set to your `allow_origins` value
   - `Access-Control-Allow-Headers` — set to `*` (all headers allowed)
   - `Access-Control-Allow-Methods` — set to `*` (all methods allowed)

2. **Handles preflight requests** — when an `OPTIONS` request comes in, the plugin responds with 200 and the CORS headers, then stops the pipeline. The request never reaches your controller.

### Per-route CORS

You might want CORS only on your API routes, not on your HTML pages. Use per-route plugins:

```erlang
routes(_Environment) ->
  [
    %% API routes with CORS enabled
    #{prefix => "/api",
      plugins => [
          {pre_request, nova_cors_plugin, #{allow_origins => <<"https://myapp.com">>}},
          {pre_request, nova_request_plugin, #{decode_json_body => true}}
      ],
      routes => [
                 {"/users", fun my_first_nova_api_controller:index/1, #{methods => [get]}},
                 {"/users", fun my_first_nova_api_controller:create/1, #{methods => [post]}},
                 {"/users/:id", fun my_first_nova_api_controller:show/1, #{methods => [get]}},
                 {"/users/:id", fun my_first_nova_api_controller:update/1, #{methods => [put]}},
                 {"/users/:id", fun my_first_nova_api_controller:delete/1, #{methods => [delete]}}
                ]
    },

    %% HTML routes without CORS
    #{prefix => "",
      security => false,
      plugins => [
          {pre_request, nova_request_plugin, #{read_urlencoded_body => true}}
      ],
      routes => [
                 {"/login", fun my_first_nova_main_controller:login/1, #{methods => [get, post]}}
                ]
    }
  ].
```

### Writing a custom CORS plugin

The built-in CORS plugin hardcodes `Allow-Headers` and `Allow-Methods` to `*`. If you need more control — specific allowed headers, credentials support, or a max-age cache — you can write your own:

```erlang
-module(my_first_nova_cors_plugin).
-behaviour(nova_plugin).

-export([pre_request/4,
         post_request/4,
         plugin_info/0]).

pre_request(Req, _Env, Options, State) ->
    Origins = maps:get(allow_origins, Options, <<"*">>),
    Methods = maps:get(allow_methods, Options, <<"GET, POST, PUT, DELETE, OPTIONS">>),
    Headers = maps:get(allow_headers, Options, <<"Content-Type, Authorization">>),
    MaxAge = maps:get(max_age, Options, <<"86400">>),

    Req1 = cowboy_req:set_resp_header(<<"access-control-allow-origin">>, Origins, Req),
    Req2 = cowboy_req:set_resp_header(<<"access-control-allow-methods">>, Methods, Req1),
    Req3 = cowboy_req:set_resp_header(<<"access-control-allow-headers">>, Headers, Req2),
    Req4 = cowboy_req:set_resp_header(<<"access-control-max-age">>, MaxAge, Req3),

    Req5 = case maps:get(allow_credentials, Options, false) of
               true ->
                   cowboy_req:set_resp_header(
                       <<"access-control-allow-credentials">>, <<"true">>, Req4);
               false ->
                   Req4
           end,

    case cowboy_req:method(Req5) of
        <<"OPTIONS">> ->
            Reply = cowboy_req:reply(204, Req5),
            {stop, Reply, State};
        _ ->
            {ok, Req5, State}
    end.

post_request(Req, _Env, _Options, State) ->
    {ok, Req, State}.

plugin_info() ->
    {<<"my_first_nova_cors_plugin">>,
     <<"1.0.0">>,
     <<"My First Nova">>,
     <<"Configurable CORS plugin">>,
     [allow_origins, allow_methods, allow_headers, max_age, allow_credentials]}.
```

Configure it with all the options you need:

```erlang
{pre_request, my_first_nova_cors_plugin, #{
    allow_origins => <<"https://myapp.com">>,
    allow_methods => <<"GET, POST, PUT, DELETE">>,
    allow_headers => <<"Content-Type, Authorization, X-Request-ID">>,
    max_age => <<"3600">>,
    allow_credentials => true
}}
```

### Testing CORS

You can verify CORS headers with curl:

```shell
# Check preflight response
$ curl -v -X OPTIONS localhost:8080/api/users \
  -H "Origin: https://myapp.com" \
  -H "Access-Control-Request-Method: POST"

# Check actual response headers
$ curl -v localhost:8080/api/users \
  -H "Origin: https://myapp.com"
```

You should see the `Access-Control-Allow-Origin` header in the response.

### Summary

- Use `nova_cors_plugin` for simple CORS with `allow_origins` config
- Set it globally in `sys.config` or per-route-group in the router
- For fine-grained control, write your own plugin with configurable methods, headers, credentials and max-age
- Always restrict `allow_origins` to your actual frontend domain in production
