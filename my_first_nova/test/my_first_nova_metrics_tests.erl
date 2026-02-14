-module(my_first_nova_metrics_tests).
-include_lib("eunit/include/eunit.hrl").

setup() ->
    {ok, Pid} = my_first_nova_metrics:start_link(),
    Pid.

cleanup(Pid) ->
    gen_server:stop(Pid).

metrics_test_() ->
    {foreach, fun setup/0, fun cleanup/1, [
        fun record_and_get_metrics/1,
        fun get_recent_returns_latest/1,
        fun empty_metrics/1,
        fun user_data_extracted/1
    ]}.

record_and_get_metrics(_Pid) ->
    fun() ->
        Metrics = #{
            req_start => erlang:monotonic_time(),
            req_end => erlang:monotonic_time(),
            resp_status => 200,
            req_body_length => 0,
            resp_body_length => 42,
            user_data => #{method => <<"GET">>, path => <<"/test">>}
        },
        my_first_nova_metrics:record(Metrics),
        timer:sleep(50),
        Summary = my_first_nova_metrics:get_metrics(),
        ?assertEqual(1, maps:get(total_requests, Summary)),
        ByStatus = maps:get(by_status, Summary),
        ?assertEqual(1, maps:get(<<"2xx">>, ByStatus))
    end.

get_recent_returns_latest(_Pid) ->
    fun() ->
        lists:foreach(
            fun(I) ->
                M = #{
                    req_start => erlang:monotonic_time(),
                    req_end => erlang:monotonic_time(),
                    resp_status => 200,
                    req_body_length => 0,
                    resp_body_length => I,
                    user_data => #{method => <<"GET">>, path => <<"/test">>}
                },
                my_first_nova_metrics:record(M),
                timer:sleep(5)
            end,
            lists:seq(1, 5)
        ),
        timer:sleep(50),
        Recent = my_first_nova_metrics:get_recent(3),
        ?assertEqual(3, length(Recent)),
        %% Should be the last 3 entries
        Lengths = [maps:get(resp_body_length, E) || E <- Recent],
        ?assertEqual([3, 4, 5], Lengths)
    end.

empty_metrics(_Pid) ->
    fun() ->
        Summary = my_first_nova_metrics:get_metrics(),
        ?assertEqual(0, maps:get(total_requests, Summary)),
        Recent = my_first_nova_metrics:get_recent(10),
        ?assertEqual([], Recent)
    end.

user_data_extracted(_Pid) ->
    fun() ->
        Metrics = #{
            req_start => erlang:monotonic_time(),
            req_end => erlang:monotonic_time(),
            resp_status => 404,
            req_body_length => 0,
            resp_body_length => 0,
            user_data => #{method => <<"POST">>, path => <<"/missing">>, tag => <<"api">>}
        },
        my_first_nova_metrics:record(Metrics),
        timer:sleep(50),
        [Entry] = my_first_nova_metrics:get_recent(1),
        ?assertEqual(<<"POST">>, maps:get(method, Entry)),
        ?assertEqual(<<"/missing">>, maps:get(path, Entry)),
        ?assertEqual(<<"api">>, maps:get(tag, Entry)),
        ?assertEqual(404, maps:get(status, Entry)),
        Summary = my_first_nova_metrics:get_metrics(),
        ByStatus = maps:get(by_status, Summary),
        ?assertEqual(1, maps:get(<<"4xx">>, ByStatus)),
        ByTag = maps:get(by_tag, Summary),
        ?assertEqual(1, maps:get(<<"api">>, ByTag))
    end.
