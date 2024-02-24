## Controller

We will now modify our controller so we can view this html.

In our application we have a controller that is in `src/controllers` the file is called `my_first_nova_contrller.erl`.

It looks like this:

```erlang
-module(my_first_nova_main_controller).
-export([
         index/1
        ]).

index(_Req) ->
    {ok, [{message, "Hello world!"}]}.
```

What this does is send `"Hello world!"` to the view that we did see when we went to `localhost:8080`. Now we want to add the function that we added in the routing section.

The function we want to add is `login/1`.

```erlang
-module(my_first_nova_main_controller).
-export([
         index/1,
         login/1
        ]).

index(_Req) ->
    {ok, [{message, "Hello world!"}]}.

login(_Req) ->
    {ok, [], #{view => login}}.
```

What we tell in the login function is that we don't have anything we want to template into out view and that this function will show the view called `login`.

