---
title: Sessions and Cookies
weight: 1
---

## Sessions and Cookies

In the authentication article we built a simple login that works with a form POST. But if you navigate to another page the auth is lost because there is no session. In this article we will look at how Nova handles sessions so we can keep users logged in across requests.

### How sessions work in Nova

Nova has a built-in session system backed by ETS (Erlang Term Storage). Session IDs are stored in a cookie called `session_id`. When a request comes in, Nova reads the cookie and you can use the session API to get and set values tied to that session.

The session manager is configured in `sys.config`:

```erlang
{nova, [
    {session_manager, nova_session_ets}
]}
```

`nova_session_ets` is the default. It stores session data in an ETS table and replicates changes across clustered nodes using `nova_pubsub`.

### The session API

Nova provides four main functions through `nova_session`:

```erlang
%% Get a value from the session
nova_session:get(Req, <<"key">>) -> {ok, Value} | {error, not_found}.

%% Set a value in the session
nova_session:set(Req, <<"key">>, <<"value">>) -> ok.

%% Delete the entire session (clears the cookie)
nova_session:delete(Req) -> {ok, Req1}.

%% Delete a specific key from the session
nova_session:delete(Req, <<"key">>) -> {ok, Req1}.

%% Generate a new session ID
nova_session:generate_session_id() -> {ok, SessionId}.
```

All functions take the Cowboy request map to read the `session_id` cookie.

### Adding sessions to our login

Let's update our auth flow to use sessions. When a user logs in, we create a session and set the cookie. On subsequent requests we check the session instead of requiring a form POST.

First update the auth module `src/my_first_nova_auth.erl`:

```erlang
-module(my_first_nova_auth).
-export([
         username_password/1,
         session_auth/1
        ]).

%% Used for the login POST
username_password(#{params := Params}) ->
    case Params of
        #{<<"username">> := Username,
          <<"password">> := <<"password">>} ->
            {true, #{authed => true, username => Username}};
        _ ->
            false
    end.

%% Used for pages that need an active session
session_auth(Req) ->
    case nova_session:get(Req, <<"username">>) of
        {ok, Username} ->
            {true, #{authed => true, username => Username}};
        {error, _} ->
            false
    end.
```

We now have two security functions. `username_password/1` handles the login form. `session_auth/1` checks if there is an active session with a username.

### Creating the session on login

Update the login controller to create a session when authentication succeeds:

```erlang
-module(my_first_nova_main_controller).
-export([
         index/1,
         login/1,
         login_post/1,
         logout/1
        ]).

index(#{auth_data := #{authed := true, username := Username}}) ->
    {ok, [{message, <<"Hello ", Username/binary>>}]};
index(_Req) ->
    {redirect, "/login"}.

login(_Req) ->
    {ok, [], #{view => login}}.

login_post(#{auth_data := #{authed := true, username := Username}} = Req) ->
    %% Generate a session ID
    {ok, SessionId} = nova_session:generate_session_id(),
    %% Set the session cookie
    Req1 = cowboy_req:set_resp_cookie(<<"session_id">>, SessionId, Req,
                                       #{path => <<"/">>, http_only => true}),
    %% Store the username in the session
    nova_session_ets:set_value(SessionId, <<"username">>, Username),
    %% Redirect to the home page
    {redirect, "/"};
login_post(_Req) ->
    {ok, [{error, <<"Invalid username or password">>}], #{view => login}}.

logout(Req) ->
    {ok, Req1} = nova_session:delete(Req),
    {redirect, "/login"}.
```

The key steps are:
1. Generate a session ID with `nova_session:generate_session_id/0`
2. Set the cookie on the response with `cowboy_req:set_resp_cookie/4`
3. Store data in the session with `nova_session_ets:set_value/3`

### Updating the routes

Now update the router to use session auth for protected pages and the login POST for the login endpoint:

```erlang
routes(_Environment) ->
  [
    %% Public routes
    #{prefix => "",
      security => false,
      routes => [
                 {"/login", fun my_first_nova_main_controller:login/1, #{methods => [get]}},
                 {"/heartbeat", fun(_) -> {status, 200} end, #{methods => [get]}}
                ]
    },

    %% Login POST (uses username/password auth)
    #{prefix => "",
      security => fun my_first_nova_auth:username_password/1,
      routes => [
                 {"/login", fun my_first_nova_main_controller:login_post/1, #{methods => [post]}}
                ]
    },

    %% Protected pages (uses session auth)
    #{prefix => "",
      security => fun my_first_nova_auth:session_auth/1,
      routes => [
                 {"/", fun my_first_nova_main_controller:index/1, #{methods => [get]}},
                 {"/logout", fun my_first_nova_main_controller:logout/1, #{methods => [get]}}
                ]
    }
  ].
```

Now the flow is:
1. User visits `/login` and sees the login form
2. Form POSTs to `/login` with username and password
3. If auth passes, a session is created and the user is redirected to `/`
4. On `/`, the `session_auth/1` function checks the session cookie
5. Visiting `/logout` deletes the session and redirects to `/login`

### Reading session data in controllers

Once a session is active, you can read values from it in any controller:

```erlang
profile(Req) ->
    case nova_session:get(Req, <<"username">>) of
        {ok, Username} ->
            {json, #{username => Username}};
        {error, _} ->
            {status, 401}
    end.
```

### Cookie options

When setting the session cookie you can pass options to control its behaviour:

```erlang
cowboy_req:set_resp_cookie(<<"session_id">>, SessionId, Req, #{
    path => <<"/">>,          %% Cookie is valid for all paths
    http_only => true,        %% Not accessible from JavaScript
    secure => true,           %% Only sent over HTTPS
    max_age => 86400          %% Expires after 24 hours (in seconds)
}).
```

For production you should always set `http_only` and `secure` to `true`.

### Custom session backends

If you want to store sessions in a database or Redis instead of ETS, you can implement the `nova_session` behaviour:

```erlang
-module(my_redis_session).
-behaviour(nova_session).

-export([start_link/0,
         get_value/2,
         set_value/3,
         delete_value/1,
         delete_value/2]).

start_link() ->
    %% Start your Redis connection
    ignore.

get_value(SessionId, Key) ->
    %% Read from Redis
    {ok, Value}.

set_value(SessionId, Key, Value) ->
    %% Write to Redis
    ok.

delete_value(SessionId) ->
    %% Delete entire session from Redis
    ok.

delete_value(SessionId, Key) ->
    %% Delete a single key from Redis
    ok.
```

Then configure it:

```erlang
{nova, [
    {session_manager, my_redis_session}
]}
```

In the next article we will look at how to write custom plugins for Nova.
