-module(my_first_nova_metrics_plugin_tests).
-include_lib("eunit/include/eunit.hrl").

pre_request_sets_method_and_path_test() ->
    Req = #{pid => self(), streamid => 1,
            method => <<"GET">>, path => <<"/api/users">>,
            headers => #{}},
    {ok, Req} = my_first_nova_metrics_plugin:pre_request(Req, #{}),
    %% cast sends a message to pid — verify it was sent
    receive
        {{_, 1}, {set_options, #{metrics_user_data := UserData}}} ->
            ?assertEqual(<<"GET">>, maps:get(method, UserData)),
            ?assertEqual(<<"/api/users">>, maps:get(path, UserData)),
            ?assertNot(maps:is_key(tag, UserData))
    after 100 ->
        ?assert(false, "Expected set_options message not received")
    end.

pre_request_includes_tag_when_configured_test() ->
    Req = #{pid => self(), streamid => 1,
            method => <<"POST">>, path => <<"/api/products">>,
            headers => #{}},
    {ok, Req} = my_first_nova_metrics_plugin:pre_request(Req, #{tag => <<"api">>}),
    receive
        {{_, 1}, {set_options, #{metrics_user_data := UserData}}} ->
            ?assertEqual(<<"POST">>, maps:get(method, UserData)),
            ?assertEqual(<<"/api/products">>, maps:get(path, UserData)),
            ?assertEqual(<<"api">>, maps:get(tag, UserData))
    after 100 ->
        ?assert(false, "Expected set_options message not received")
    end.

post_request_passes_through_test() ->
    Req = #{method => <<"GET">>, path => <<"/">>},
    ?assertEqual({ok, Req}, my_first_nova_metrics_plugin:post_request(Req, #{})).

plugin_info_test() ->
    {Title, Version, _Author, _Desc, Opts} = my_first_nova_metrics_plugin:plugin_info(),
    ?assertEqual(<<"my_first_nova_metrics_plugin">>, Title),
    ?assertEqual(<<"1.0.0">>, Version),
    ?assert(length(Opts) > 0).
