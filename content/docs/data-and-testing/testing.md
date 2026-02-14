---
title: "Testing"
weight: 3
---
## Testing Nova Applications

Nova applications can be tested with Erlang's built-in testing tools: EUnit for unit tests and Common Test for integration tests. The [nova_test](https://github.com/novaframework/nova_test) library provides helpers that make both styles of testing more convenient: a request builder for unit testing controllers, an HTTP client for integration tests and assertion macros for common checks.

### Adding nova_test

Add `nova_test` as a test dependency in `rebar.config`:

```erlang
{profiles, [
    {test, [
        {deps, [
            {nova_test, "0.1.0"}
        ]}
    ]}
]}.
```

### EUnit - Unit testing controllers

EUnit is Erlang's built-in unit testing framework. Since Nova controllers are regular Erlang functions that receive a Cowboy request map and return a tuple, they are straightforward to unit test. The `nova_test_req` module provides a builder for constructing request maps and `nova_test.hrl` includes assertion macros for matching controller return values.

Create a test file `test/my_first_nova_api_controller_tests.erl`:

```erlang
-module(my_first_nova_api_controller_tests).
-include_lib("nova_test/include/nova_test.hrl").

show_existing_user_test() ->
    Req = nova_test_req:new(get, "/users/1"),
    Req1 = nova_test_req:with_bindings(#{<<"id">> => <<"1">>}, Req),
    Result = my_first_nova_api_controller:show(Req1),
    ?assertJsonResponse(#{id := 1, name := _, email := _}, Result).

show_missing_user_test() ->
    Req = nova_test_req:new(get, "/users/999999"),
    Req1 = nova_test_req:with_bindings(#{<<"id">> => <<"999999">>}, Req),
    Result = my_first_nova_api_controller:show(Req1),
    ?assertStatusResponse(404, Result).

create_user_test() ->
    Req = nova_test_req:new(post, "/users"),
    Req1 = nova_test_req:with_json(#{<<"name">> => <<"Alice">>,
                                     <<"email">> => <<"alice@example.com">>}, Req),
    Result = my_first_nova_api_controller:create(Req1),
    ?assertJsonResponse(201, #{id := _}, Result).

create_missing_params_test() ->
    Req = nova_test_req:new(post, "/users"),
    Req1 = nova_test_req:with_json(#{}, Req),
    Result = my_first_nova_api_controller:create(Req1),
    ?assertStatusResponse(422, Result).
```

The `nova_test_req:new/2` function creates a well-formed Cowboy request map with sensible defaults. You then add bindings, JSON bodies, headers, query parameters and more using the `with_*` functions:

| Function | Purpose |
|---|---|
| `nova_test_req:with_bindings/2` | Set path bindings (e.g. `#{<<"id">> => <<"1">>}`) |
| `nova_test_req:with_json/2` | Set a JSON body (auto-encodes and sets content-type) |
| `nova_test_req:with_header/3` | Add a request header |
| `nova_test_req:with_query/2` | Set query string parameters |
| `nova_test_req:with_body/2` | Set a raw body |
| `nova_test_req:with_auth_data/2` | Set auth data (for testing authenticated controllers) |
| `nova_test_req:with_peer/2` | Set the client peer address |

Run the tests with:

```shell
$ rebar3 eunit
```

The challenge with testing controllers that depend on the database is that you need the database running. For pure unit tests you can extract the logic and test it separately.

### Testing without database dependency

A better approach for unit tests is to separate logic from data access. You can test request parsing and response formatting without hitting the database:

```erlang
-module(my_first_nova_request_tests).
-include_lib("nova_test/include/nova_test.hrl").

parse_json_body_test() ->
    Req = nova_test_req:new(post, "/users"),
    Req1 = nova_test_req:with_json(#{<<"name">> => <<"Alice">>,
                                     <<"email">> => <<"alice@example.com">>}, Req),
    #{json := #{<<"name">> := Name, <<"email">> := Email}} = Req1,
    ?assertEqual(<<"Alice">>, Name),
    ?assertEqual(<<"alice@example.com">>, Email).

parse_bindings_test() ->
    Req = nova_test_req:new(get, "/users/42"),
    Req1 = nova_test_req:with_bindings(#{<<"id">> => <<"42">>}, Req),
    #{bindings := #{<<"id">> := Id}} = Req1,
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

Common Test is Erlang's integration testing framework. It is suited for testing the full HTTP stack. The `nova_test` module provides an HTTP client that handles application startup, port discovery and JSON encoding so you can focus on writing tests.

You can scaffold a Common Test suite with the `rebar3_nova` generator:

```shell
$ rebar3 nova gen_test --name users
===> Writing test/my_first_nova_users_controller_SUITE.erl
```

This creates a suite with test cases for each CRUD action (list, show, create, update, delete) that make HTTP requests against your running application. It is a good starting point that you can extend with your own assertions. See the [Code Generators](../../developer-tools/code-generators/) article for the full details.

Here is what a hand-written suite looks like using `nova_test`. Create the test suite `test/my_first_nova_api_SUITE.erl`:

```erlang
-module(my_first_nova_api_SUITE).
-include_lib("common_test/include/ct.hrl").
-include_lib("nova_test/include/nova_test.hrl").

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
    nova_test:start(my_first_nova, Config).

end_per_suite(Config) ->
    nova_test:stop(Config).

test_get_users(Config) ->
    {ok, Resp} = nova_test:get("/api/users", Config),
    ?assertStatus(200, Resp),
    ?assertJson(#{<<"users">> := [_ | _]}, Resp).

test_create_user(Config) ->
    {ok, Resp} = nova_test:post("/api/users",
                                #{json => #{<<"name">> => <<"Test User">>,
                                            <<"email">> => <<"test@example.com">>}},
                                Config),
    ?assertStatus(201, Resp),
    ?assertJson(#{<<"name">> := <<"Test User">>}, Resp).

test_get_user_not_found(Config) ->
    {ok, Resp} = nova_test:get("/api/users/999999", Config),
    ?assertStatus(404, Resp).
```

`nova_test:start/2` boots your application, discovers the port from Nova's cowboy configuration and returns an updated Config. All HTTP functions (`get`, `post`, `put`, `patch`, `delete`) accept a path, optional options and the Config.

The assertion macros from `nova_test.hrl` work on the response map:

| Macro | Purpose |
|---|---|
| `?assertStatus(Code, Resp)` | Assert the HTTP status code |
| `?assertJson(Pattern, Resp)` | Pattern-match the decoded JSON body |
| `?assertBody(Expected, Resp)` | Assert the raw response body |
| `?assertHeader(Name, Expected, Resp)` | Assert a response header value (case-insensitive) |

Run the Common Test suite with:

```shell
$ rebar3 ct
```

### Testing security modules

You can also unit test your security modules directly using `nova_test_req` to build requests:

```erlang
-module(my_first_nova_auth_tests).
-include_lib("nova_test/include/nova_test.hrl").

valid_login_test() ->
    Req = nova_test_req:new(post, "/login"),
    Req1 = nova_test_req:with_json(#{<<"username">> => <<"admin">>,
                                     <<"password">> => <<"password">>}, Req),
    ?assertMatch({true, #{authed := true, username := <<"admin">>}},
                 my_first_nova_auth:username_password(Req1)).

invalid_password_test() ->
    Req = nova_test_req:new(post, "/login"),
    Req1 = nova_test_req:with_json(#{<<"username">> => <<"admin">>,
                                     <<"password">> => <<"wrong">>}, Req),
    ?assertEqual(false, my_first_nova_auth:username_password(Req1)).

missing_params_test() ->
    Req = nova_test_req:new(post, "/login"),
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
- Use `nova_test_req` to build proper Cowboy request maps instead of constructing them by hand
- Use the assertion macros from `nova_test.hrl` for readable test assertions
- Keep database-dependent tests in Common Test suites where you can set up and tear down test data
- Use `-ifdef(TEST)` to export helper functions only in test builds
- Run `rebar3 eunit` for unit tests and `rebar3 ct` for integration tests
- You can run both with `rebar3 do eunit, ct`

In the next article we will tie everything together and build a complete CRUD application.
