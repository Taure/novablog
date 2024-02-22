## Controllers

We have talked about plugins, routing, and basic security modules.

Now we will discuss where the logic of the application resides and how to write our controllers. We did see in earlier articles that we could template data into our views as an example. Now we will show how you can return status codes, different headers or encode to json.

### Controller flow

![Image description](https://dev-to-uploads.s3.amazonaws.com/uploads/articles/v21alnncsiqd9p6rkjmq.png)

### Handlers

Handlers are the modules in Nova that handle the controller response. It is here where we decide if you return `{json, #{hello => world}}` that Nova will JSON encode the body and return an HTTP code with 200.

### Return values

In a controller, you can return the data in different ways and Nova will handle them differently.

`{ok, [{message, Message}]}`: because we use the ok tuple here Nova will try to find a view that matches your controller name and template the data into it.

`{status, StatusCode, Header, Body}`: This will return more raw data, here you need to encode the Body to a format you want to return, and also add the headers you want.

`{json, StatusCode, Header, Body}`: In this scenario the body is an Erlang map and Nova will encode it into a json.

For more return values, you can find them in the documentation [here](https://hexdocs.pm/nova/controllers.html#json-structures).

### Fallback controllers

Phoenix has a really useful feature called action_fallback. We thought it would be a good addition to Nova to include something similar, and therefore the fallback_controller was introduced. If a controller returns an invalid/unhandled result, the fallback controller gets invoked and can take action on the payload. It's good for separating error handling from the controllers. A fallback controller is set by setting the fallback_controller attribute with the module name of the fallback controller.

The following example shows how a controller defines a fallback:


```erlang
-module(my_main_controller).
-export([
    error_example/1
]).
-fallback_controller(my_fallback_controller).

error_example(_Req) ->
    %% Since {error, ...} is not a valid handler the fallback-controller will be invoked
    {error, example_error}.
```

A fallback controller exposes one function resolve/2 which returns a handler (like for regular controllers) to return the response to the client. If we take the previous example and try to build a fallback controller for it:

```erlang
-module(my_fallback_controller).
-export([
    resolve/2
]).

resolve(Req, {error, example_error}) ->
  {status, 400}.
```
