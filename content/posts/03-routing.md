---
title: "Routing in Nova"
date: 2024-07-15
weight: 3
tags: ["routing"]
series: ["Nova Framework Guide"]
series_order: 3
summary: "How routing works — prefixes, security, and environment-specific routes."
---
## Routing

Here I will write about routing in Nova and how it works.

When we generate a new Nova app we will have a routing file within our src directory.

The file is named in our example `my_first_nova_router.erl` in that file we have a function that has a list of Erlang maps that will specify and configure what rules are for an endpoint.

```erlang
-module(my_first_nova_router).
-behaviour(nova_router).

-export([
         routes/1
        ]).

%% The Environment-variable is defined in your sys.config in {nova, [{environment, Value}]}
routes(_Environment) ->
    [#{prefix => "",
      security => false,
      routes => [
                 {"/", fun my_first_nova_main_controller:index/1 , #{methods => [get]}},
                 {"/heartbeat", fun(_) -> {status, 200} end, #{methods => [get]}}
                ]
      }].

```

What does this routing configuration indicate?

If we run this we will see the routing tree in your shell:
```shell
$ rebar3 nova routes
Host: '_'
     ├─  /assets
        └─  _ /[...] (my_first_nova, cowboy_static:init/1)
     └─  GET / (my_first_nova, my_first_nova_main_controller:index/1)

```

`_Environement` is a variable that is set in the config file, in `dev_sys.config.src` we can find `{environment, dev}` and for `prod_sys.config.src` we have `{environment, prod}`. In this file that we got from the start we don't care if the environment is dev or prod we will start the routes here for all environments. It could be so when we develop that we want to have some extra routes for dev environment that we don't want to have in production. Then we can add a new function for dev and add the routes.

Example:
```erlang
-module(my_first_nova_router).
-behaviour(nova_router).

-export([
         routes/1
        ]).

%% The Environment-variable is defined in your sys.config in {nova, [{environment, Value}]}
routes(prod) ->
  prod();
routes(dev) ->
  prod() ++ dev().

prod() ->
  [#{prefix => "",
      security => false,
      routes => [
                 {"/", fun my_first_nova_main_controller:index/1, #{methods => [get]}},       
                 {"/heartbeat", fun(_) -> {status, 200} end, #{methods => [get]}}
                ]
      }].

dev() ->
  [#{prefix => "",
  security => false,
  routes => [
             {"/dev-tools", fun my_first_nova_dev_controller:index/1, #{methods => [get]}}
            ]
  }].
```

If you run `rebar3 nova routes` you will see that you get same result before we added the dev routes. This is because we only show what is in production and not in your development routes.

Back to the structure of the routing:
```erlang
#{prefix => "",
  security => false,
  routes => [
             {"/", fun my_first_nova_main_controller:index/1 , #{methods => [get]}},
             {"/heartbeat", fun(_) -> {status, 200} end, #{methods => [get]}}
            ]
  }
```
`prefix` is if you want to group your routes, example if you are going to make a lot of routes for users `v1/user`, `v1/user/USERKEY`, `v1/user/USERKEY/...` then you can use a prefix with `prefix => "/v1/user"` and every route in routes field will have it as a prefix.
`security` here is if we want to have any security for the routes specified in this map. `security` takes a Erlang function, `fun Module:Function/1`.
`routes` this is a list that takes an Erlang tuple `{PATH, fun Module:Function/1, #{}}`. Path is the endpoint, the function with `Module`and `Function` will tell you what controller is used for this endpoint.

What we want to do is to add a new route to our application:

```erlang
-module(my_first_nova_router).
-behaviour(nova_router).

-export([
         routes/1
        ]).

%% The Environment-variable is defined in your sys.config in {nova, [{environment, Value}]}
routes(_Environment) ->
  [#{prefix => "",
      security => false,
      routes => [
                 {"/", fun my_first_nova_main_controller:index/1 , #{methods => [get]}},
                 {"/heartbeat", fun(_) -> {status, 200} end, #{methods => [get]}},
                 {"/login", fun my_first_nova_main_controller:login/1, #{methods => [get]}}
                ]
      }].
```

In the example above we have added a new route for `login`.
