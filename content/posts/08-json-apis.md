---
title: "Building a JSON API"
date: 2024-08-19T00:00:00+00:00
weight: 8
tags: ["nova", "erlang", "json", "rest", "api"]
series: ["Nova Framework Guide"]
summary: "Building REST API endpoints that return JSON responses."
---

## Building a JSON API with Nova

So far in this series we have been rendering HTML views with ErlyDTL templates. But what if we want to build a REST API that returns JSON? Nova makes this straightforward.

### JSON handler

Nova has a built-in JSON handler. Instead of returning `{ok, Variables}` from your controller (which renders a template), you return `{json, Data}` and Nova will encode it and set the correct content-type header.

Let's create a new controller. In `src/controllers` create a file called `my_first_nova_api_controller.erl`:

```erlang
-module(my_first_nova_api_controller).
-export([
         index/1,
         show/1,
         create/1
        ]).

index(_Req) ->
    Users = [
        #{id => 1, name => <<"Alice">>, email => <<"alice@example.com">>},
        #{id => 2, name => <<"Bob">>, email => <<"bob@example.com">>}
    ],
    {json, #{users => Users}}.

show(#{bindings := #{<<"id">> := Id}}) ->
    {json, #{id => binary_to_integer(Id), name => <<"Alice">>, email => <<"alice@example.com">>}};
show(_Req) ->
    {status, 400, #{}, #{error => <<"missing id">>}}.

create(#{params := #{<<"name">> := Name, <<"email">> := Email}}) ->
    {json, 201, #{}, #{id => 3, name => Name, email => Email}};
create(_Req) ->
    {status, 422, #{}, #{error => <<"name and email required">>}}.
```

Let's look at what is happening here.

`index/1` returns a list of users as JSON. The `{json, Data}` tuple tells Nova to encode the map as JSON and respond with status 200.

`show/1` uses `bindings` from the request map. When we define a route with a path parameter like `"/users/:id"`, Nova will put the matched value in the bindings map.

`create/1` uses `params` to read the decoded request body. We return `{json, 201, #{}, Data}` which lets us set a custom status code. The third element is a map of extra headers if we need them.

### Adding the routes

Now let's add the routes in our `my_first_nova_router.erl`. We will use a prefix to group our API routes:

```erlang
-module(my_first_nova_router).
-behaviour(nova_router).

-export([
         routes/1
        ]).

routes(_Environment) ->
  [#{prefix => "",
      security => false,
      routes => [
                 {"/login", fun my_first_nova_main_controller:login/1, #{methods => [get]}},
                 {"/heartbeat", fun(_) -> {status, 200} end, #{methods => [get]}}
                ]
      },
    #{prefix => "",
      security => fun my_first_nova_auth:username_password/1,
      routes => [{"/", fun my_first_nova_main_controller:index/1, #{methods => [post]}}]
     },
    #{prefix => "/api",
      security => false,
      routes => [
                 {"/users", fun my_first_nova_api_controller:index/1, #{methods => [get]}},
                 {"/users/:id", fun my_first_nova_api_controller:show/1, #{methods => [get]}},
                 {"/users", fun my_first_nova_api_controller:create/1, #{methods => [post]}}
                ]
     }
   ].
```

We added a new route map with `prefix => "/api"`. All routes in this group will be prefixed with `/api`, so the full paths become `/api/users` and `/api/users/:id`.

### Configuring JSON decoding

For our POST endpoint we need Nova to decode the incoming JSON body. We update the plugin configuration in `dev_sys.config.src`:

```erlang
{plugins, [
    {pre_request, nova_request_plugin, #{
        decode_json_body => true,
        read_urlencoded_body => true
    }}
]}
```

With `decode_json_body => true`, the `nova_request_plugin` will decode incoming JSON bodies and put them in the `params` key of the request map. We keep `read_urlencoded_body` for our login form.

### JSON library

Nova uses `thoas` as the default JSON library. If you want to use a different library like `jsx` or `jiffy`, you can configure it in your application environment:

```erlang
{my_first_nova, [
    {json_lib, jsx}
]}
```

The library module needs to export `encode/1` and `decode/1`.

### Testing our API

Start the node and let's test with curl:

```shell
$ rebar3 nova serve
```

Get all users:
```shell
$ curl -s localhost:8080/api/users | python3 -m json.tool
{
    "users": [
        {
            "id": 1,
            "name": "Alice",
            "email": "alice@example.com"
        },
        {
            "id": 2,
            "name": "Bob",
            "email": "bob@example.com"
        }
    ]
}
```

Get a single user:
```shell
$ curl -s localhost:8080/api/users/1 | python3 -m json.tool
{
    "id": 1,
    "name": "Alice",
    "email": "alice@example.com"
}
```

Create a user:
```shell
$ curl -s -X POST localhost:8080/api/users \
  -H "Content-Type: application/json" \
  -d '{"name": "Charlie", "email": "charlie@example.com"}' | python3 -m json.tool
{
    "id": 3,
    "name": "Charlie",
    "email": "charlie@example.com"
}
```

### Response formats

Here is a summary of the different return tuples you can use for JSON responses:

```erlang
%% Simple JSON response (200 for GET, 201 for POST)
{json, #{key => value}}

%% JSON with custom status code
{json, StatusCode, Headers, Body}

%% Status response (also encodes maps as JSON)
{status, StatusCode}
{status, StatusCode, Headers, Body}

%% Redirect
{redirect, "/some/path"}
```

### Adding custom headers

If you need to add custom headers to your response, use the headers map:

```erlang
index(_Req) ->
    {json, 200, #{<<"x-request-id">> => <<"abc123">>}, #{users => []}}.
```

In the next article we will look at WebSockets and how Nova handles real-time communication.
