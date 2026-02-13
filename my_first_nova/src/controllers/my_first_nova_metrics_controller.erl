-module(my_first_nova_metrics_controller).

-export([summary/1,
         recent/1]).

summary(_Req) ->
    Metrics = my_first_nova_metrics:get_metrics(),
    %% Convert durations list to avg/p99 for JSON output
    Durations = maps:get(durations_us, Metrics, []),
    Stats = duration_stats(Durations),
    {json, maps:without([durations_us], Metrics#{duration_stats_us => Stats})}.

recent(_Req) ->
    Recent = my_first_nova_metrics:get_recent(20),
    {json, #{requests => Recent}}.

%% Internal

duration_stats([]) ->
    #{avg => 0, p99 => 0, max => 0};
duration_stats(Durations) ->
    Sorted = lists:sort(Durations),
    Len = length(Sorted),
    Avg = lists:sum(Sorted) div Len,
    P99Index = max(1, ceil(Len * 0.99)),
    P99 = lists:nth(P99Index, Sorted),
    Max = lists:last(Sorted),
    #{avg => Avg, p99 => P99, max => Max}.
