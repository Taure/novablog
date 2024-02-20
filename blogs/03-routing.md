Here I will write about routing in Nova and how it works.

When we generate a new Nova app we will have a routing file in our src directory.

The file is named `MYAPP_router.erl` in that file we have a function that has a list of Erlang maps that will specify and configure what rules are for an endpoint.

```
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
                 {"/", { my_first_nova_main_controller, index}, #{methods => [get]}},
                 {"/assets/[...]", "assets"}
                ]
      },
      #{prefix => "",
      security => {my_first_nova_auth, auth},
      plugins => [{pre_request, nova_request_plugin, #{read_urlencoded_body => true}}],
      routes => [
           {"/login", { my_first_nova_login_controller, index}, #{methods => [post]}}
               ]
    }].
```

What does this routing configuration say to us?
The prefix is what we will match against first, in this way, you can create different routing configures depending on the prefix.

```
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
                 {"/", { my_first_nova_main_controller, index}, #{methods => [get]}},
                 {"/assets/[...]", "assets"}
                ]
      },
      #{prefix => "",
      security => {my_first_nova_auth, auth},
      plugins => [{pre_request, nova_request_plugin, #{read_urlencoded_body => true}}],
      routes => [
                 {"/login", { my_first_nova_login_controller, index}, #{methods => [post]}}
               ]
    }],
    #{prefix => "/user/:userid",
      security => {my_first_nova_auth, auth},
      routes => [
                {"/", { my_first_nova_user_controller, get_user}, #{methods => [get]}},
                {"/pet", {my_first_nova_user_controller, get_user_pet}, #{methods => [get]}}
            ]
}.

```
The last map here is using prefix, so all paths in routes will be `/user/:userid` and then the endpoint in the routes.
