---
title: "WebSockets and Handlers"
date: 2024-08-26T00:00:00+00:00
weight: 9
tags: ["nova", "erlang", "websockets", "real-time"]
series: ["Nova Framework Guide"]
summary: "Real-time communication with WebSockets and Nova's handler system."
---

## WebSockets and Handlers

In previous articles we have been working with HTTP requests and responses. But sometimes you need real-time communication between the server and the client. This is where WebSockets come in.

Nova has built-in support for WebSockets through the `nova_websocket` behaviour.

### Creating a WebSocket handler

A WebSocket handler is a module that implements the `nova_websocket` behaviour. It needs to export at least three functions: `init/1`, `websocket_handle/2` and `websocket_info/2`.

Let's create a simple echo WebSocket handler. Create a file `src/controllers/my_first_nova_ws_handler.erl`:

```erlang
-module(my_first_nova_ws_handler).
-behaviour(nova_websocket).

-export([
         init/1,
         websocket_handle/2,
         websocket_info/2
        ]).

init(State) ->
    {ok, State}.

websocket_handle({text, Msg}, State) ->
    {reply, {text, <<"Echo: ", Msg/binary>>}, State};
websocket_handle(_Frame, State) ->
    {ok, State}.

websocket_info(_Info, State) ->
    {ok, State}.
```

Let's go through the callbacks:

`init/1` is called when the WebSocket connection is established. It receives a state map that contains the Cowboy request. You return `{ok, State}` to accept the connection.

`websocket_handle/2` is called when a message is received from the client. The first argument is the frame, for text messages it will be `{text, Binary}`. We pattern match on it and reply with an echo. You can return:
- `{ok, State}` to do nothing
- `{reply, Frame, State}` to send a message back
- `{stop, State}` to close the connection

`websocket_info/2` is called when the handler process receives an Erlang message (not a WebSocket frame). This is useful for receiving messages from other processes, like pub/sub notifications. We will use this in a later article.

### Adding the route

WebSocket routes look a bit different from HTTP routes. Instead of passing a fun reference, we pass the module name as an atom and set `protocol => ws` in the options:

```erlang
#{prefix => "",
  security => false,
  routes => [
             {"/ws", my_first_nova_ws_handler, #{protocol => ws}}
            ]
}
```

Add this to your router. The full router now looks like:

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
                 {"/heartbeat", fun(_) -> {status, 200} end, #{methods => [get]}},
                 {"/ws", my_first_nova_ws_handler, #{protocol => ws}}
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

### Testing the WebSocket

Start the node with `rebar3 nova serve`. You can test the WebSocket from a browser console:

```javascript
let ws = new WebSocket("ws://localhost:8080/ws");
ws.onmessage = (e) => console.log(e.data);
ws.onopen = () => ws.send("Hello Nova!");
// Should log: "Echo: Hello Nova!"
```

### A more useful example - Chat handler

Let's build something more practical. A simple chat handler where messages are broadcast to all connected clients. Create `src/controllers/my_first_nova_chat_handler.erl`:

```erlang
-module(my_first_nova_chat_handler).
-behaviour(nova_websocket).

-export([
         init/1,
         websocket_handle/2,
         websocket_info/2
        ]).

init(State) ->
    nova_pubsub:join(chat),
    {ok, State}.

websocket_handle({text, Msg}, State) ->
    nova_pubsub:broadcast(chat, "message", Msg),
    {ok, State};
websocket_handle(_Frame, State) ->
    {ok, State}.

websocket_info({nova_pubsub, chat, _Sender, "message", Msg}, State) ->
    {reply, {text, Msg}, State};
websocket_info(_Info, State) ->
    {ok, State}.
```

In `init/1` we join the `chat` channel using `nova_pubsub`. When a client sends a message, we broadcast it to all members of the channel. When we receive a pub/sub message in `websocket_info/2`, we send it to the connected client. We will dive deeper into pub/sub in a later article.

### Custom handlers

Nova has a handler registry that maps return tuple atoms to handler functions. The built-in handlers are:

| Return atom | What it does |
|---|---|
| `json` | Encodes data as JSON |
| `ok` | Renders an ErlyDTL template |
| `status` | Returns a status code |
| `redirect` | Redirects to another URL |
| `sendfile` | Sends a file |
| `view` | Renders a specific view template |

You can register your own handlers if you need custom response processing:

```erlang
nova_handlers:register_handler(xml, fun my_xml_handler:handle/3).
```

Then in your controller you can return:

```erlang
my_action(_Req) ->
    {xml, <<"<user><name>Alice</name></user>">>}.
```

The handler function receives `(StatusCode, ExtraHeaders, ControllerPayload)` and must return a Cowboy request.

### Fallback controllers

If a controller returns something unexpected, Nova can fall back to a module that handles the error. You set this with a module attribute:

```erlang
-module(my_controller).
-fallback_controller(my_fallback).

-export([index/1]).

index(_Req) ->
    something_unexpected.
```

The fallback module needs to export `resolve/2`:

```erlang
-module(my_fallback).
-export([resolve/2]).

resolve(Req, InvalidReturn) ->
    logger:warning("Invalid return from controller: ~p", [InvalidReturn]),
    {status, 500, #{}, #{error => <<"internal server error">>}}.
```

In the next article we will look at database integration and how to persist data in a Nova application.
