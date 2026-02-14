%%%-------------------------------------------------------------------
%% @doc my_first_nova top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(my_first_nova_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    Metrics = #{
        id => my_first_nova_metrics,
        start => {my_first_nova_metrics, start_link, []},
        restart => permanent,
        type => worker
    },
    {ok, {{one_for_all, 0, 1}, [Metrics]}}.

%%====================================================================
%% Internal functions
%%====================================================================
