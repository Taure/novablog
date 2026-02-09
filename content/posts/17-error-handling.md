---
title: "Error Handling"
date: 2024-10-21
weight: 17
tags: ["errors", "error-handling"]
series: ["Nova Framework Guide"]
series_order: 17
summary: "Custom error pages, status code routes, and fallback controllers."
---
## Error Handling

When something goes wrong in your application you want to show a useful error page instead of a cryptic error. In this article we will look at how Nova handles errors and how to create custom error pages.

### Nova's default error handling

Nova comes with default handlers for 404 (not found) and 500 (server error) responses. These are registered in Nova's own router and work out of the box. In development mode, 500 errors will show crash details. In production they return a bare status code.

### Status code routes

Nova lets you register custom handlers for specific HTTP status codes directly in your router. The format is the same as a regular route but with a status code integer instead of a path:

```erlang
routes(_Environment) ->
  [#{routes => [
        {404, fun my_first_nova_error_controller:not_found/1, #{}},
        {500, fun my_first_nova_error_controller:server_error/1, #{}}
     ]},
   #{prefix => "",
     security => false,
     routes => [
                {"/", fun my_first_nova_main_controller:index/1, #{methods => [get]}},
                {"/heartbeat", fun(_) -> {status, 200} end, #{methods => [get]}}
               ]
   }].
```

Your status code handlers override Nova's defaults because your application's routes are compiled after Nova's built-in routes.

### Creating an error controller

Create `src/controllers/my_first_nova_error_controller.erl`:

```erlang
-module(my_first_nova_error_controller).
-export([
         not_found/1,
         server_error/1
        ]).

not_found(_Req) ->
    {ok, [{title, <<"404 - Not Found">>},
          {message, <<"The page you are looking for does not exist.">>}],
     #{view => error_page, status_code => 404}}.

server_error(_Req) ->
    {ok, [{title, <<"500 - Server Error">>},
          {message, <<"Something went wrong. Please try again later.">>}],
     #{view => error_page, status_code => 500}}.
```

The `status_code` option in the return map tells Nova what HTTP status code to send with the response.

### Error view template

Create `src/views/error_page.dtl`:

```html
<html>
<head><title>{{ title }}</title></head>
<body>
  <h1>{{ title }}</h1>
  <p>{{ message }}</p>
  <a href="/">Go back home</a>
</body>
</html>
```

Now when a user hits a URL that doesn't exist they will see a proper error page instead of a bare 404 response.

### JSON error responses

If you are building an API you probably want JSON error responses instead of HTML. You can check the `Accept` header to decide:

```erlang
not_found(Req) ->
    case cowboy_req:header(<<"accept">>, Req) of
        <<"application/json">> ->
            {json, 404, #{}, #{error => <<"not_found">>,
                               message => <<"Resource not found">>}};
        _ ->
            {ok, [{title, <<"404">>}, {message, <<"Page not found">>}],
             #{view => error_page, status_code => 404}}
    end.
```

### Handling controller crashes

When a controller function crashes (throws an exception), Nova catches it and triggers the 500 error handler. The request map passed to your error controller will contain `crash_info` with details about what went wrong.

In development you might want to log the crash:

```erlang
server_error(#{crash_info := CrashInfo} = _Req) ->
    logger:error("Controller crash: ~p", [CrashInfo]),
    {ok, [{title, <<"500">>},
          {message, <<"Internal server error">>}],
     #{view => error_page, status_code => 500}};
server_error(_Req) ->
    {ok, [{title, <<"500">>},
          {message, <<"Internal server error">>}],
     #{view => error_page, status_code => 500}}.
```

### Adding more status codes

You can register handlers for any HTTP status code:

```erlang
routes(_Environment) ->
  [#{routes => [
        {400, fun my_first_nova_error_controller:bad_request/1, #{}},
        {401, fun my_first_nova_error_controller:unauthorized/1, #{}},
        {403, fun my_first_nova_error_controller:forbidden/1, #{}},
        {404, fun my_first_nova_error_controller:not_found/1, #{}},
        {500, fun my_first_nova_error_controller:server_error/1, #{}}
     ]},
   %% ... your regular routes
  ].
```

And implement them in the error controller:

```erlang
bad_request(_Req) ->
    {json, 400, #{}, #{error => <<"bad_request">>}}.

unauthorized(_Req) ->
    {json, 401, #{}, #{error => <<"unauthorized">>}}.

forbidden(_Req) ->
    {json, 403, #{}, #{error => <<"forbidden">>}}.
```

### Disabling error page rendering

If you want Nova to skip its error page rendering entirely and just return bare status codes, you can set this in your config:

```erlang
{nova, [
    {render_error_pages, false}
]}
```

This is useful if you handle all error responses in your controllers and don't want Nova's error resolution chain to interfere.

### Error handling in the pipeline

Here is the full picture of how errors flow through Nova:

1. **Route not found** — Nova triggers the 404 handler
2. **Security function returns false** — Nova triggers the 401 handler
3. **Controller crashes** — Nova catches the exception and triggers the 500 handler
4. **Plugin returns `{error, Reason}`** — Nova triggers the 500 handler
5. **Controller returns `{status, Code}`** — If a handler is registered for that code, it is used

For each case, Nova looks up your registered status code handler. If none is registered, it falls back to its own default handler.

### Fallback controllers

There is another error handling mechanism worth mentioning. If a controller returns an unexpected value (not a recognized tuple like `{json, ...}` or `{ok, ...}`), Nova can delegate to a fallback controller. Set it with a module attribute:

```erlang
-module(my_first_nova_api_controller).
-fallback_controller(my_first_nova_error_controller).

-export([index/1]).

index(_Req) ->
    %% If this returns something unexpected, the fallback handles it
    case do_something() of
        {ok, Data} -> {json, Data};
        unexpected_value -> unexpected_value  %% Goes to fallback
    end.
```

The fallback module needs a `resolve/2` function:

```erlang
resolve(Req, InvalidReturn) ->
    logger:warning("Unexpected controller return: ~p", [InvalidReturn]),
    {status, 500, #{}, #{error => <<"internal server error">>}}.
```

This is a safety net that catches programming errors and turns them into proper responses.

In the next article we will look at CORS and how to configure cross-origin requests for your API.
