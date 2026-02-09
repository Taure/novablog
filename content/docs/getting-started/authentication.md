---
title: "Adding Authentication"
weight: 6
---
## Adding auth

In earlier sections, we have talked about routings, plugins and views and touched controllers.

Now we will add security so we can try our login view.

Security is managed within our routing file, that will point to a security module and function. We have our routing file `my_first_nova_router.erl` looking like this:

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

We will now re-arragne some of the routing so we can add the security, ususally we store things as auth tokens and use that to do next api call. But this is just an simple example so I will just add auth to our path and let our view make a post to it. We will show the Username in our homepage.

We will start with adding a new Route map into the list, and move the endpoint `/` this one will use a security module that we will create that will be called `my_first_nova_auth.erl` this module will have a function called `username_password`. The new route map will look like this:

```erlang
    #{prefix => "",
      security => fun my_first_nova_auth:username_password/1,
      routes => [{"/", fun my_first_nova_main_controller:index/1, #{methods => [post]}}]
     }
```
Here we have added the security `{my_first_nova_auth, username_password}` so this will be called before the request hits the controller.

Lets create our security module, in `src` directory you can create `my_first_nova_auth.erl`, when we talked about plugins we configured our plugin to hand urlencoded body, this is what our form will give us.

```erlang
-module(my_first_nova_auth).

-export([username_password/1]).

username_password(#{params := Params}) ->
    case Params of
        #{<<"username">> := Username,
          <<"password">> := <<"password">>} -> {true, #{authed => true,
                                                         username => Username}};
        _ -> false
    end.
```

What we will also do just to be sure that only authed user will see our page we will add a flag that we will set if someone passed the auth. Here we also change the message back to the view and add the Username that we get from our auth module. If you want only some username to be authed we could add a list of username that we could see if they are a member in.

```erlang
index(#{auth_data := #{authed := true,
                       username := Username}}) ->
    {ok, [{message, <<"Hello ", Username/binary>>}]};
index(_Req) ->
    {status, 401}.
```

This will see if we have `#{authed => true}` in our request, this is set from the return of the security module that we will cre


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
                 {"/login", fun my_first_nova_main_controller:login/1, #{methods => [get]}},
                 {"/heartbeat", fun(_) -> {status, 200} end, #{methods => [get]}}
                ]
      },
    #{prefix => "",
      security => fun my_first_nova_auth:username_password/1,
      routes => [{"/", fun my_first_nova_main_controller:index/1, #{methods => [post]}}]
     }
   ].

```

Now we can try this out, start the node `rebar3 nova serve` then we can go to `localhost:8080/login` this will show us our view with username and password form. Try any username and the password is `password`. If everything works you will see the start page with starts and Nova loggo saying `Hello USERNAME`.
