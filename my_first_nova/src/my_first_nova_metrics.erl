-module(my_first_nova_metrics).
-behaviour(gen_server).

-export([start_link/0,
         record/1,
         get_metrics/0,
         get_recent/1]).

-export([init/1,
         handle_call/3,
         handle_cast/2]).

-define(TABLE, my_first_nova_metrics_tab).
-define(MAX_RECENT, 100).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

record(Metrics) ->
    gen_server:cast(?MODULE, {record, Metrics}).

get_metrics() ->
    gen_server:call(?MODULE, get_metrics).

get_recent(N) ->
    gen_server:call(?MODULE, {get_recent, N}).

%% gen_server callbacks

init([]) ->
    ets:new(?TABLE, [named_table, ordered_set, protected]),
    {ok, #{counter => 0}}.

handle_call(get_metrics, _From, State = #{counter := Count}) ->
    Entries = ets:tab2list(?TABLE),
    Summary = build_summary(Entries),
    {reply, Summary#{total_requests => Count}, State};
handle_call({get_recent, N}, _From, State) ->
    Recent = take_last(?TABLE, N),
    {reply, Recent, State};
handle_call(_Msg, _From, State) ->
    {reply, ok, State}.

handle_cast({record, Metrics}, #{counter := Count} = State) ->
    Entry = extract_entry(Metrics),
    Key = {erlang:monotonic_time(), Count},
    ets:insert(?TABLE, {Key, Entry}),
    prune(?TABLE),
    {noreply, State#{counter => Count + 1}};
handle_cast(_Msg, State) ->
    {noreply, State}.

%% Internal

extract_entry(Metrics) ->
    UserData = maps:get(user_data, Metrics, #{}),
    ReqStart = maps:get(req_start, Metrics, 0),
    ReqEnd = maps:get(req_end, Metrics, ReqStart),
    DurationUs = erlang:convert_time_unit(ReqEnd - ReqStart, native, microsecond),
    #{
        method => maps:get(method, UserData, unknown),
        path => maps:get(path, UserData, unknown),
        tag => maps:get(tag, UserData, undefined),
        status => maps:get(resp_status, Metrics, 0),
        duration_us => DurationUs,
        req_body_length => maps:get(req_body_length, Metrics, 0),
        resp_body_length => maps:get(resp_body_length, Metrics, 0),
        timestamp => erlang:system_time(second)
    }.

build_summary(Entries) ->
    lists:foldl(fun({_Key, Entry}, Acc) ->
        #{status := Status, duration_us := Dur, tag := Tag} = Entry,
        StatusGroup = status_group(Status),
        ByStatus = maps:get(by_status, Acc, #{}),
        ByTag = maps:get(by_tag, Acc, #{}),
        Durations = maps:get(durations_us, Acc, []),
        Acc#{
            by_status => maps:update_with(StatusGroup, fun(V) -> V + 1 end, 1, ByStatus),
            by_tag => case Tag of
                undefined -> ByTag;
                _ -> maps:update_with(Tag, fun(V) -> V + 1 end, 1, ByTag)
            end,
            durations_us => [Dur | Durations]
        }
    end, #{by_status => #{}, by_tag => #{}, durations_us => []}, Entries).

status_group(S) when S >= 200, S < 300 -> <<"2xx">>;
status_group(S) when S >= 300, S < 400 -> <<"3xx">>;
status_group(S) when S >= 400, S < 500 -> <<"4xx">>;
status_group(S) when S >= 500 -> <<"5xx">>;
status_group(_) -> <<"other">>.

take_last(Table, N) ->
    take_last(Table, ets:last(Table), N, []).

take_last(_Table, '$end_of_table', _N, Acc) ->
    Acc;
take_last(_Table, _Key, 0, Acc) ->
    Acc;
take_last(Table, Key, N, Acc) ->
    [{_, Entry}] = ets:lookup(Table, Key),
    take_last(Table, ets:prev(Table, Key), N - 1, [Entry | Acc]).

prune(Table) ->
    Size = ets:info(Table, size),
    case Size > ?MAX_RECENT of
        true ->
            FirstKey = ets:first(Table),
            ets:delete(Table, FirstKey),
            prune(Table);
        false ->
            ok
    end.
