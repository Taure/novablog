Plugins in Nova are modules that have a behaviour. These behaviours will be a part of the pipeline flow of a request.

```
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

Plugins can be used on HTTP requests. The example above shows two functions that have, pre_request (That will happen before the controller) and post_request (That will happen after the controller).

### Prioritization & Configuration

We want to say in what order we want plugins to be run. This we will do in sys.config where we also can set extra options for the plugins.

```
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
                    {pre_http_request, nova_request_plugin, #{decode_json_body => true,
                                          parse_qs => true}}
                   ]}
        ]},
```
In this case:
`nova_cors_plugin` is first
`nova_request_plugin` will be called after `nova_cors_plugin`
In the nova_request_plugin we have set some values:

```
{pre_http_request, nova_request_plugin, #{decode_json_body => true,
                                          parse_qs => true}}
```

What will the request plugin do?

We have decode_json_body this will if we get a body in the request and it it content-type: application/json decode the body and move it to the Request object.

```
#{json => JSONMap} (Keys in JSONMap will be binary)
```

The last part of the request plugin will add QS to Nova state if QS is used.

```
#{
  json => JSONMap,
  parsed_qs => QS}
```

Because we have this in a plugin this will happen for all requests that are used for this Nova application. So, we don't need to handle JSON decode for all of our controllers.

Plugins are a way to remove things that you would need to do in all your requests for your endpoints. It can be either all endpoints or some of them depending on how you add the plugins to your routes.

### Plugins vs Cowboy middlewares

If you have used Cowboy you know that you can write middlewares where it does some things for all your requests depending on what order you have the middlewares.

Plugins are very similar, but you can have on module now that handles what happens before the request arrives at your controller and what happens after your controller.

The difference is that you can say what endpoints are going to use what plugins and in that way do more specific things like decoding body or json validation for example.