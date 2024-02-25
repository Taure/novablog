## Plugins

![Image description](https://dev-to-uploads.s3.amazonaws.com/uploads/articles/v21alnncsiqd9p6rkjmq.png)


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
         {use_stacktrace, true},
         {environment, dev},
         {cowboy_configuration, #{
                                  port => 8080
                                 }},
         {dev_mode, true},
         {bootstrap_application, my_first_nova}, %% Bootstraps the application
         %% Plugins is written on form {RequestType, Module, Options, Priority}
         %% Priority is that the lowest number is executed first
         {plugins, [
                    {pre_request, nova_request_plugin, #{decode_json_body => true}}
                   ]}
        ]}
  %% Please change your app.src-file instead if you intend to add app-specific configurations
```
In this scenario:
`nova_request_plugin` is a plugin that is included with Nova, it can handle some decoding of incomming bodies.

To read more about what plugins are included in Nova you can read it [here](https://hexdocs.pm/nova/plugins.html).

What we want to do in our application is that we are later going to create a view that will send in an urlencoded body from a form submit. An easy login page.

If we want Nova to decode this urlencoded body we will need to change our plugin setting to:

```erlang
    {plugins, [
            {pre_request, nova_request_plugin, #{read_urlencoded_body => true}}
            ]}
```
