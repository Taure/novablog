---
title: "Writing Custom Plugins"
weight: 2
---
## Writing Custom Plugins

In the plugins article we looked at how Nova's built-in plugins work. In this article we will build our own plugins from scratch. A plugin lets you run code before or after every request — logging, rate limiting, adding headers, or anything else you need in the pipeline.

### The nova_plugin behaviour

A plugin module must implement the `nova_plugin` behaviour. The callbacks are:

```erlang
-callback pre_request(Req, Env, Options, State) ->
    {ok, Req, State} |      %% Continue to the next plugin
    {break, Req, State} |   %% Skip remaining plugins, go to controller
    {stop, Req, State} |    %% Stop entirely, plugin handles the response
    {error, Reason}.        %% Trigger a 500 error

-callback post_request(Req, Env, Options, State) ->
    {ok, Req, State} |
    {break, Req, State} |
    {stop, Req, State} |
    {error, Reason}.

-callback plugin_info() ->
    {Title, Version, Author, Description, Options}.
```

You implement `pre_request` for code that runs before the controller and `post_request` for code that runs after. `plugin_info` returns metadata about your plugin.

### Example 1: Request logger

Let's build a plugin that logs every incoming request with the method, path and response time.

Create `src/plugins/my_first_nova_logger_plugin.erl`:

```erlang
-module(my_first_nova_logger_plugin).
-behaviour(nova_plugin).

-include_lib("kernel/include/logger.hrl").

-export([pre_request/4,
         post_request/4,
         plugin_info/0]).

pre_request(Req, _Env, _Options, State) ->
    StartTime = erlang:monotonic_time(millisecond),
    {ok, Req#{start_time => StartTime}, State}.

post_request(Req, _Env, _Options, State) ->
    StartTime = maps:get(start_time, Req, 0),
    Duration = erlang:monotonic_time(millisecond) - StartTime,
    Method = cowboy_req:method(Req),
    Path = cowboy_req:path(Req),
    ?LOG_INFO("~s ~s completed in ~pms", [Method, Path, Duration]),
    {ok, Req, State}.

plugin_info() ->
    {<<"my_first_nova_logger_plugin">>,
     <<"1.0.0">>,
     <<"My First Nova">>,
     <<"Logs request method, path and duration">>,
     []}.
```

In `pre_request` we record the start time by adding it to the request map. In `post_request` we calculate the duration and log it.

Register the plugin in `dev_sys.config.src`:

```erlang
{plugins, [
    {pre_request, nova_request_plugin, #{decode_json_body => true,
                                          read_urlencoded_body => true}},
    {pre_request, my_first_nova_logger_plugin, #{}},
    {post_request, my_first_nova_logger_plugin, #{}}
]}
```

Note that we register it twice — once as `pre_request` and once as `post_request`. This is because the plugin pipeline runs pre_request plugins before the controller and post_request plugins after.

Now when you make requests you will see log output like:

```
[info] GET /api/users completed in 3ms
[info] POST /api/users completed in 12ms
```

### Example 2: Rate limiter

Let's build a simple rate limiter that limits requests per IP address. This one uses ETS to track request counts.

Create `src/plugins/my_first_nova_rate_limit_plugin.erl`:

```erlang
-module(my_first_nova_rate_limit_plugin).
-behaviour(nova_plugin).

-export([pre_request/4,
         post_request/4,
         plugin_info/0]).

pre_request(Req, _Env, Options, State) ->
    MaxRequests = maps:get(max_requests, Options, 100),
    WindowMs = maps:get(window_ms, Options, 60000),
    {IP, _Port} = cowboy_req:peer(Req),
    Key = {rate_limit, IP},
    Now = erlang:monotonic_time(millisecond),
    case ets:lookup(nova_rate_limits, Key) of
        [{Key, Count, WindowStart}] when Now - WindowStart < WindowMs ->
            if Count >= MaxRequests ->
                    Reply = cowboy_req:reply(429,
                        #{<<"content-type">> => <<"application/json">>},
                        <<"{\"error\":\"too many requests\"}">>,
                        Req),
                    {stop, Reply, State};
               true ->
                    ets:update_element(nova_rate_limits, Key, {2, Count + 1}),
                    {ok, Req, State}
            end;
        _ ->
            ets:insert(nova_rate_limits, {Key, 1, Now}),
            {ok, Req, State}
    end.

post_request(Req, _Env, _Options, State) ->
    {ok, Req, State}.

plugin_info() ->
    {<<"my_first_nova_rate_limit_plugin">>,
     <<"1.0.0">>,
     <<"My First Nova">>,
     <<"Simple IP-based rate limiting">>,
     [max_requests, window_ms]}.
```

We need to create the ETS table on application start. Add it to `src/my_first_nova_app.erl`:

```erlang
-module(my_first_nova_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    ets:new(nova_rate_limits, [named_table, public, set]),
    my_first_nova_sup:start_link().

stop(_State) ->
    ok.
```

Configure it in `sys.config`:

```erlang
{plugins, [
    {pre_request, nova_request_plugin, #{decode_json_body => true}},
    {pre_request, my_first_nova_rate_limit_plugin, #{
        max_requests => 60,
        window_ms => 60000
    }}
]}
```

This limits each IP to 60 requests per minute. When the limit is exceeded the plugin returns `{stop, Reply, State}` which sends a 429 response and skips the controller entirely.

### Example 3: Request ID plugin

A plugin that adds a unique request ID header to every response, useful for tracing and debugging:

```erlang
-module(my_first_nova_request_id_plugin).
-behaviour(nova_plugin).

-export([pre_request/4,
         post_request/4,
         plugin_info/0]).

pre_request(Req, _Env, _Options, State) ->
    RequestId = generate_id(),
    Req1 = cowboy_req:set_resp_header(<<"x-request-id">>, RequestId, Req),
    {ok, Req1#{request_id => RequestId}, State}.

post_request(Req, _Env, _Options, State) ->
    {ok, Req, State}.

plugin_info() ->
    {<<"my_first_nova_request_id_plugin">>,
     <<"1.0.0">>,
     <<"My First Nova">>,
     <<"Adds X-Request-ID header to responses">>,
     []}.

generate_id() ->
    Bytes = crypto:strong_rand_bytes(16),
    list_to_binary(
        lists:flatten(
            [io_lib:format("~2.16.0b", [B]) || <<B:8>> <= Bytes])).
```

### Example 4: Metrics enrichment with Cowboy

Cowboy has a built-in metrics stream handler called `cowboy_metrics_h`. It collects timing, status codes and body sizes for every request, then calls your callback with a metrics map when the request finishes. The interesting part is that you can **push custom data into those metrics from your plugin** using `cowboy_req:cast/2`.

This is powerful because it lets your plugins tag requests with business-level metadata — route names, user IDs, feature flags — and have it arrive automatically in your metrics pipeline.

#### How it works

1. You add `cowboy_metrics_h` to the Cowboy stream handler chain
2. You provide a `metrics_callback` function that receives metrics at the end of each request
3. From any plugin (or handler), you call `cowboy_req:cast({set_options, #{metrics_user_data => YourMap}}, Req)` to attach custom data
4. The custom data shows up in the `user_data` field of the metrics map delivered to your callback

The `cast` sends a message to the connection process, which flows through the stream handler chain. `cowboy_metrics_h` intercepts `set_options` commands and merges `metrics_user_data` into its accumulated state. You can call it multiple times — values are merged.

#### Setting up the stream handler

First, configure Cowboy to include `cowboy_metrics_h` and a callback. In `dev_sys.config.src`:

```erlang
{cowboy_configuration, #{
    port => 8080,
    stream_handlers => [cowboy_metrics_h, nova_stream_h,
                        cowboy_compress_h, cowboy_stream_h],
    options => #{
        compress => true,
        metrics_callback => fun my_first_nova_metrics:record/1
    }
}}
```

`cowboy_metrics_h` must come before `cowboy_stream_h` in the chain so it wraps the full request lifecycle.

#### The metrics collector

We need somewhere for the callback to send data. Here is a simple gen_server backed by ETS:

```erlang
-module(my_first_nova_metrics).
-behaviour(gen_server).

-export([start_link/0, record/1, get_metrics/0, get_recent/1]).
-export([init/1, handle_call/3, handle_cast/2]).

-define(TABLE, my_first_nova_metrics_tab).
-define(MAX_RECENT, 100).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% This is the metrics_callback — called by cowboy_metrics_h
record(Metrics) ->
    gen_server:cast(?MODULE, {record, Metrics}).

get_metrics() ->
    gen_server:call(?MODULE, get_metrics).

get_recent(N) ->
    gen_server:call(?MODULE, {get_recent, N}).

init([]) ->
    ets:new(?TABLE, [named_table, ordered_set, protected]),
    {ok, #{counter => 0}}.

handle_cast({record, Metrics}, #{counter := Count} = State) ->
    UserData = maps:get(user_data, Metrics, #{}),
    ReqStart = maps:get(req_start, Metrics, 0),
    ReqEnd = maps:get(req_end, Metrics, ReqStart),
    DurationUs = erlang:convert_time_unit(ReqEnd - ReqStart, native, microsecond),
    Entry = #{
        method => maps:get(method, UserData, unknown),
        path => maps:get(path, UserData, unknown),
        tag => maps:get(tag, UserData, undefined),
        status => maps:get(resp_status, Metrics, 0),
        duration_us => DurationUs,
        resp_body_length => maps:get(resp_body_length, Metrics, 0)
    },
    Key = {erlang:monotonic_time(), Count},
    ets:insert(?TABLE, {Key, Entry}),
    {noreply, State#{counter => Count + 1}};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_call(get_metrics, _From, State = #{counter := Count}) ->
    Entries = ets:tab2list(?TABLE),
    {reply, #{total_requests => Count, entries => Entries}, State};
handle_call({get_recent, N}, _From, State) ->
    %% Take last N entries from ordered_set
    Recent = take_last(?TABLE, ets:last(?TABLE), N, []),
    {reply, Recent, State};
handle_call(_Msg, _From, State) ->
    {reply, ok, State}.

take_last(_Table, '$end_of_table', _N, Acc) -> Acc;
take_last(_Table, _Key, 0, Acc) -> Acc;
take_last(Table, Key, N, Acc) ->
    [{_, Entry}] = ets:lookup(Table, Key),
    take_last(Table, ets:prev(Table, Key), N - 1, [Entry | Acc]).
```

Start it under your supervisor so it is running before Cowboy starts handling requests.

#### The plugin

Now we write the plugin that pushes data into the metrics stream:

```erlang
-module(my_first_nova_metrics_plugin).
-behaviour(nova_plugin).

-export([pre_request/2, post_request/2, plugin_info/0]).

pre_request(Req, Options) ->
    Method = cowboy_req:method(Req),
    Path = cowboy_req:path(Req),
    UserData0 = #{method => Method, path => Path},
    UserData = case maps:get(tag, Options, undefined) of
        undefined -> UserData0;
        Tag -> UserData0#{tag => Tag}
    end,
    cowboy_req:cast({set_options, #{metrics_user_data => UserData}}, Req),
    {ok, Req}.

post_request(Req, _Options) ->
    {ok, Req}.

plugin_info() ->
    {<<"my_first_nova_metrics_plugin">>,
     <<"1.0.0">>,
     <<"My First Nova">>,
     <<"Enriches Cowboy metrics with request metadata">>,
     [{tag, <<"Optional tag to label routes in metrics">>}]}.
```

The key line is `cowboy_req:cast({set_options, #{metrics_user_data => UserData}}, Req)`. This sends a message to the connection process carrying our custom data. `cowboy_metrics_h` picks it up and merges it into the `user_data` map that arrives in the `record/1` callback.

#### Register the plugin

Add it to your plugins list in `sys.config`:

```erlang
{plugins, [
    {pre_request, nova_request_plugin, #{decode_json_body => true}},
    {pre_request, my_first_nova_metrics_plugin, #{}}
]}
```

Or tag specific route groups:

```erlang
#{prefix => "/api",
  plugins => [
      {pre_request, my_first_nova_metrics_plugin, #{tag => <<"api">>}}
  ],
  routes => [...]
}
```

#### Seeing it in action

Start the application and make some requests:

```shell
curl http://localhost:8080/api/users
curl http://localhost:8080/api/products
curl http://localhost:8080/api/metrics/recent
```

The last request returns the collected metrics including your custom data:

```json
{
  "requests": [
    {
      "method": "GET",
      "path": "/api/users",
      "tag": "api",
      "status": 200,
      "duration_us": 1234,
      "resp_body_length": 95
    },
    {
      "method": "GET",
      "path": "/api/products",
      "status": 200,
      "duration_us": 987,
      "resp_body_length": 142
    }
  ]
}
```

The `method`, `path` and `tag` fields all came from the plugin via `metrics_user_data`. The `status`, `duration_us` and `resp_body_length` came from Cowboy's built-in metrics collection. Both arrive together in a single callback.

#### Why this matters

The standard approach of measuring things inside your handler (like the logger plugin in Example 1) only captures the handler's processing time. The `cowboy_metrics_h` approach captures the **full request lifecycle** — from when Cowboy starts parsing the request to when the last byte of the response is sent. It also captures body sizes, subprocess timing and informational responses automatically.

By combining this with plugins that tag requests via `metrics_user_data`, you get production-grade observability without instrumenting every handler individually.

### Per-route plugins

You can also set plugins on specific route groups instead of globally. This is useful when you want different behaviour for your API versus your HTML pages:

```erlang
routes(_Environment) ->
  [
    #{prefix => "/api",
      plugins => [
          {pre_request, nova_cors_plugin, #{allow_origins => <<"*">>}},
          {pre_request, nova_request_plugin, #{decode_json_body => true}},
          {pre_request, my_first_nova_rate_limit_plugin, #{max_requests => 100}}
      ],
      routes => [
                 {"/users", fun my_first_nova_api_controller:index/1, #{methods => [get]}}
                ]
    },

    #{prefix => "",
      plugins => [
          {pre_request, nova_request_plugin, #{read_urlencoded_body => true}}
      ],
      routes => [
                 {"/login", fun my_first_nova_main_controller:login/1, #{methods => [get, post]}}
                ]
    }
  ].
```

When `plugins` is set on a route group, it overrides the global plugin configuration for those routes.

### Plugin return values

Here is a summary of what each return value does:

| Return | Effect |
|---|---|
| `{ok, Req, State}` | Continue to the next plugin or controller |
| `{break, Req, State}` | Skip remaining plugins in this phase, go to controller |
| `{stop, Req, State}` | Stop everything, the plugin must have already sent a response |
| `{error, Reason}` | Trigger a 500 error page |

The `stop` return is the most powerful — it lets plugins short-circuit the entire request pipeline. We used this in the rate limiter to reject requests before they reach the controller.

In the next article we will look at error handling and custom error pages.
