## Plugins

Plugins in Nova are modules that have a behaviour, these behaviours will be a part of the pipeline flow of a request.

```erlang
-module(nova_correlation_plugin).

-behaviour(nova_plugin).

-export([pre_request/2,
         post_request/2,
         plugin_info/0]).

-include_lib("kernel/include/logger.hrl").

%%--------------------------------------------------------------------
%% @doc
%% Pre-request callback to either pick up correlation ID from request headers
%% or generate a new uuid correlation ID.
%% @end
%%--------------------------------------------------------------------

pre_request(Req0, Opts) ->
    CorrId = get_correlation_id(Req0, Opts),
    %% Update the logger's metadata with correlation-id
    ok = update_logger_metadata(CorrId, Opts),
    Req1 = cowboy_req:set_resp_header(<<"X-Correlation-ID">>, CorrId, Req0),
    Req = Req1#{correlation_id => CorrId},
    {ok, Req}.

post_request(Req, _) ->
    {ok, Req}.

plugin_info() ->
   {
     <<"nova_correlation_plugin">>,
     <<"0.2.0">>,
     <<"Nova team <info@novaframework.org">>,
     <<"Add X-Correlation-ID headers to response">>,
     []
    }.

get_correlation_id(Req, #{ request_correlation_header := CorrelationHeader }) ->
    case cowboy_req:header(CorrelationHeader, Req) of
        undefined ->
            uuid();
        CorrId ->
            CorrId
    end;

get_correlation_id(_Req, _Opts) ->
    uuid().

uuid() ->
    uuid:uuid_to_string(uuid:get_v4()).

update_logger_metadata(CorrId, Opts) ->
    LoggerKey = maps:get(logger_metadata_key, Opts, <<"correlation-id">>),
    logger:update_process_metadata(#{LoggerKey => CorrId}).
```

This is an example plugin that will add a correlation ID to all your requests.

## Pipeline

We can look at the flow of a request as a pipeline that will go into different plugins before and after it has done the controller code.

Plugins can be used for HTTP requests. The example above shows two functions, such as pre_request (That will happen before the controller) and post_request (That will happen after the controller).

### Prioritization & Configuration

We want to say in what order we want plugins to be run. We do this in sys.config where we also can set extra options for the plugins.

```erlang
{nova, [
         {cowboy_configuration, #{
                                  port => 8190
                                 }},
         {dev_mode, true},
         {bootstrap_application, MYAPP}, %% Bootstraps the application
         %% Plugins is written on form {RequestType, Module, Options, Priority}
         %% Priority is that the lowest number is executed first
         {plugins, [
                    {pre_request, nova_cors_plugin, #{}},
                    {pre_request, nova_request_plugin, #{decode_json_body => true,
                                          parse_qs => true}}
                   ]}
        ]},
```
In this scenario:
`nova_cors_plugin` is first,
`nova_request_plugin` will be called after `nova_cors_plugin`
In the nova_request_plugin, we have set some values:

```erlang
{pre_request, nova_request_plugin, #{decode_json_body => true,
                                          parse_qs => true}}
```

What will the request plugin do?

We have decode_json_body this will if we get a body in the request and it it content-type: application/json decode the body and move it to the Request object.

```erlang
#{json => JSONMap} (Keys in JSONMap will be binary)
```

The last part of the request plugin will add QS to Nova state if QS is used.

```erlang
#{
  json => JSONMap,
  parsed_qs => QS}
```

Because we have this in a plugin this will happen for all requests that are used for this Nova application. So, we don't need to handle JSON decode for all of our controllers.

Plugins are a way to streamline processes that you would otherwise need to perform in all your requests for your endpoints. They can apply to either all endpoints or specific ones, depending on how you add the plugins to your routes.

### Plugins vs Cowboy middlewares

If you've used Cowboy, you know that you can write middlewares that perform certain actions for all your requests, depending on their order.

Plugins are very similar, but now you can have one module that handles what happens both before and after the request reaches your controller.

The difference is that you can say what endpoints are going to use what plugins and in that way do more specific things like decoding body or json validation for example.