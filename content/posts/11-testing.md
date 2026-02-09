---
title: "Testing Nova Applications"
date: 2024-09-09
weight: 11
tags: ["testing", "eunit", "common-test"]
series: ["Nova Framework Guide"]
series_order: 11
summary: "Unit testing with EUnit and integration testing with Common Test."
---
## Testing Nova Applications

Nova does not ship with its own testing framework. Instead we use Erlang's built-in testing tools: EUnit for unit tests and Common Test for integration tests. In this article we will look at how to test controllers, repository modules and HTTP endpoints.

### EUnit - Unit testing controllers

EUnit is Erlang's built-in unit testing framework. It is great for testing individual functions. Since Nova controllers are just regular Erlang functions that receive a request map and return a tuple, they are easy to unit test.

Create a test file `test/my_first_nova_api_controller_tests.erl`:

```erlang
-module(my_first_nova_api_controller_tests).
-include_lib("eunit/include/eunit.hrl").

show_existing_user_test() ->
    %% Assuming user with id 1 exists in the database
    Req = #{bindings => #{<<"id">> => <<"1">>}},
    Result = my_first_nova_api_controller:show(Req),
    ?assertMatch({json, #{id := 1, name := _, email := _}}, Result).

show_missing_user_test() ->
    Req = #{bindings => #{<<"id">> => <<"999999">>}},
    Result = my_first_nova_api_controller:show(Req),
    ?assertMatch({status, 404, _, _}, Result).

create_missing_params_test() ->
    Req = #{params => #{}},
    Result = my_first_nova_api_controller:create(Req),
    ?assertMatch({status, 422, _, _}, Result).
```

Run the tests with:

```shell
$ rebar3 eunit
```

The challenge with testing controllers that depend on the database is that you need the database running. For pure unit tests you can extract the logic and test it separately.

### Testing without database dependency

A better approach for unit tests is to separate logic from data access. We can test the request parsing and response formatting without hitting the database:

```erlang
-module(my_first_nova_request_tests).
-include_lib("eunit/include/eunit.hrl").

parse_user_params_test() ->
    Req = #{params => #{<<"name">> => <<"Alice">>, <<"email">> => <<"alice@example.com">>}},
    #{params := #{<<"name">> := Name, <<"email">> := Email}} = Req,
    ?assertEqual(<<"Alice">>, Name),
    ?assertEqual(<<"alice@example.com">>, Email).

parse_bindings_test() ->
    Req = #{bindings => #{<<"id">> => <<"42">>}},
    #{bindings := #{<<"id">> := Id}} = Req,
    ?assertEqual(42, binary_to_integer(Id)).

row_to_map_test() ->
    Row = {1, <<"Alice">>, <<"alice@example.com">>},
    Expected = #{id => 1, name => <<"Alice">>, email => <<"alice@example.com">>},
    ?assertEqual(Expected, my_first_nova_user_repo:row_to_map(Row)).
```

Note that for the `row_to_map` test to work, you need to export the function. You can use the `-ifdef(TEST)` guard to export it only for tests:

```erlang
-module(my_first_nova_user_repo).

-export([all/0, get/1, create/2, update/3, delete/1]).

-ifdef(TEST).
-export([row_to_map/1]).
-endif.
```

### Common Test - Integration testing

Common Test is Erlang's integration testing framework. It is more powerful than EUnit and is suited for testing the full HTTP stack. We will use `gun` as an HTTP client to make requests to our running Nova application.

Add `gun` as a test dependency in `rebar.config`:

```erlang
{profiles, [
    {test, [
        {deps, [
            {gun, "2.1.0"}
        ]}
    ]}
]}.
```

Create the test suite `test/my_first_nova_api_SUITE.erl`:

```erlang
-module(my_first_nova_api_SUITE).
-include_lib("common_test/include/ct.hrl").

-export([
         all/0,
         init_per_suite/1,
         end_per_suite/1,
         test_get_users/1,
         test_create_user/1,
         test_get_user_not_found/1
        ]).

all() ->
    [test_get_users,
     test_create_user,
     test_get_user_not_found].

init_per_suite(Config) ->
    application:ensure_all_started(my_first_nova),
    {ok, ConnPid} = gun:open("localhost", 8080),
    {ok, _Protocol} = gun:await_up(ConnPid),
    [{conn, ConnPid} | Config].

end_per_suite(Config) ->
    ConnPid = proplists:get_value(conn, Config),
    gun:close(ConnPid),
    ok.

test_get_users(Config) ->
    ConnPid = proplists:get_value(conn, Config),
    StreamRef = gun:get(ConnPid, "/api/users"),
    {response, nofin, 200, _Headers} = gun:await(ConnPid, StreamRef),
    {ok, Body} = gun:await_body(ConnPid, StreamRef),
    {ok, Json} = thoas:decode(Body),
    true = maps:is_key(<<"users">>, Json).

test_create_user(Config) ->
    ConnPid = proplists:get_value(conn, Config),
    Body = thoas:encode(#{name => <<"Test User">>, email => <<"test@example.com">>}),
    StreamRef = gun:post(ConnPid, "/api/users",
                         [{<<"content-type">>, <<"application/json">>}],
                         Body),
    {response, nofin, 201, _Headers} = gun:await(ConnPid, StreamRef),
    {ok, RespBody} = gun:await_body(ConnPid, StreamRef),
    {ok, Json} = thoas:decode(RespBody),
    <<"Test User">> = maps:get(<<"name">>, Json).

test_get_user_not_found(Config) ->
    ConnPid = proplists:get_value(conn, Config),
    StreamRef = gun:get(ConnPid, "/api/users/999999"),
    {response, nofin, 404, _Headers} = gun:await(ConnPid, StreamRef),
    ok.
```

Run the Common Test suite with:

```shell
$ rebar3 ct
```

### Testing security modules

You can also unit test your security modules directly:

```erlang
-module(my_first_nova_auth_tests).
-include_lib("eunit/include/eunit.hrl").

valid_login_test() ->
    Req = #{params => #{<<"username">> => <<"admin">>,
                        <<"password">> => <<"password">>}},
    ?assertMatch({true, #{authed := true, username := <<"admin">>}},
                 my_first_nova_auth:username_password(Req)).

invalid_password_test() ->
    Req = #{params => #{<<"username">> => <<"admin">>,
                        <<"password">> => <<"wrong">>}},
    ?assertEqual(false, my_first_nova_auth:username_password(Req)).

missing_params_test() ->
    Req = #{params => #{}},
    ?assertEqual(false, my_first_nova_auth:username_password(Req)).
```

### Project structure for tests

Your test directory should mirror your source structure:

```
test/
├── my_first_nova_api_controller_tests.erl   %% EUnit
├── my_first_nova_auth_tests.erl             %% EUnit
├── my_first_nova_request_tests.erl          %% EUnit
└── my_first_nova_api_SUITE.erl              %% Common Test
```

### Tips

- Use EUnit for fast unit tests of individual functions
- Use Common Test for integration tests that need the full application running
- Keep database-dependent tests in Common Test suites where you can set up and tear down test data
- Use `-ifdef(TEST)` to export helper functions only in test builds
- Run `rebar3 eunit` for unit tests and `rebar3 ct` for integration tests
- You can run both with `rebar3 do eunit, ct`

In the next article we will tie everything together and build a complete CRUD application.
