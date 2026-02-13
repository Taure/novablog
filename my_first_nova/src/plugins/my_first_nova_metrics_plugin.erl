-module(my_first_nova_metrics_plugin).
-behaviour(nova_plugin).

-export([pre_request/2,
         post_request/2,
         plugin_info/0]).

pre_request(Req, Options) ->
    Method = cowboy_req:method(Req),
    Path = cowboy_req:path(Req),
    UserData0 = #{
        method => Method,
        path => Path
    },
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
     <<"Enriches Cowboy metrics with request metadata via metrics_user_data">>,
     [{tag, <<"Optional tag to label routes in metrics">>}]}.
