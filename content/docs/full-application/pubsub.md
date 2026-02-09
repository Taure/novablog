---
title: Pub/Sub
weight: 3
---

## Pub/Sub in Nova

In the WebSocket article we briefly used `nova_pubsub` to broadcast chat messages. In this article we will dive deeper into Nova's pub/sub system and build a real-time notification feature for our notes application.

### How nova_pubsub works

Nova's pub/sub system is built on top of Erlang's `pg` module (process groups). It is started automatically with Nova, no configuration needed. Any Erlang process can join channels, and messages are delivered to all members of a channel.

The API is simple:

```erlang
%% Join a channel
nova_pubsub:join(channel_name).

%% Leave a channel
nova_pubsub:leave(channel_name).

%% Broadcast to all members on all nodes
nova_pubsub:broadcast(channel_name, Topic, Payload).

%% Broadcast to members on the local node only
nova_pubsub:local_broadcast(channel_name, Topic, Payload).

%% Get all members of a channel
nova_pubsub:get_members(channel_name).

%% Get members on the local node
nova_pubsub:get_local_members(channel_name).
```

Channels are atoms. Topics can be lists or binaries. Payloads can be anything.

### Message format

When a process receives a pub/sub message, it arrives as a tuple:

```erlang
{nova_pubsub, Channel, SenderPid, Topic, Payload}
```

In a gen_server or WebSocket handler, you handle this in `handle_info/2` or `websocket_info/2`.

### Building real-time notifications

Let's add real-time notifications to our notes app. When someone creates, updates or deletes a note, all connected clients get notified.

#### The notification WebSocket handler

Create `src/controllers/my_first_nova_notifications_handler.erl`:

```erlang
-module(my_first_nova_notifications_handler).
-behaviour(nova_websocket).

-export([
         init/1,
         websocket_handle/2,
         websocket_info/2
        ]).

init(State) ->
    nova_pubsub:join(notes),
    {ok, State}.

websocket_handle({text, <<"ping">>}, State) ->
    {reply, {text, <<"pong">>}, State};
websocket_handle(_Frame, State) ->
    {ok, State}.

websocket_info({nova_pubsub, notes, _Sender, Topic, Payload}, State) ->
    Msg = thoas:encode(#{
        event => list_to_binary(Topic),
        data => Payload
    }),
    {reply, {text, Msg}, State};
websocket_info(_Info, State) ->
    {ok, State}.
```

When a client connects, the handler joins the `notes` channel. When a pub/sub message arrives it encodes the event and data as JSON and sends it to the client.

#### Broadcasting from controllers

Now we update our notes API controller to broadcast events when notes change:

```erlang
-module(my_first_nova_notes_api_controller).
-export([
         index/1,
         show/1,
         create/1,
         update/1,
         delete/1
        ]).

index(_Req) ->
    {ok, Notes} = my_first_nova_note_repo:all(),
    {json, #{notes => Notes}}.

show(#{bindings := #{<<"id">> := Id}}) ->
    case my_first_nova_note_repo:get(binary_to_integer(Id)) of
        {ok, Note} ->
            {json, Note};
        {error, not_found} ->
            {status, 404, #{}, #{error => <<"note not found">>}}
    end.

create(#{params := #{<<"title">> := Title, <<"body">> := Body,
                     <<"author">> := Author}}) ->
    case my_first_nova_note_repo:create(Title, Body, Author) of
        {ok, Note} ->
            nova_pubsub:broadcast(notes, "note_created", Note),
            {json, 201, #{}, Note};
        {error, Reason} ->
            {status, 422, #{}, #{error => list_to_binary(io_lib:format("~p", [Reason]))}}
    end;
create(_Req) ->
    {status, 422, #{}, #{error => <<"title, body and author required">>}}.

update(#{bindings := #{<<"id">> := Id},
         params := #{<<"title">> := Title, <<"body">> := Body}}) ->
    case my_first_nova_note_repo:update(binary_to_integer(Id), Title, Body) of
        {ok, Note} ->
            nova_pubsub:broadcast(notes, "note_updated", Note),
            {json, Note};
        {error, not_found} ->
            {status, 404, #{}, #{error => <<"note not found">>}}
    end;
update(_Req) ->
    {status, 422, #{}, #{error => <<"title and body required">>}}.

delete(#{bindings := #{<<"id">> := Id}}) ->
    IntId = binary_to_integer(Id),
    case my_first_nova_note_repo:delete(IntId) of
        ok ->
            nova_pubsub:broadcast(notes, "note_deleted", #{id => IntId}),
            {status, 204};
        {error, not_found} ->
            {status, 404, #{}, #{error => <<"note not found">>}}
    end.
```

The only change is adding `nova_pubsub:broadcast/3` calls after successful operations.

#### Adding the route

Add the WebSocket route to the router:

```erlang
{"/notifications", my_first_nova_notifications_handler, #{protocol => ws}}
```

#### Client-side JavaScript

Here is a simple JavaScript client that connects to the notification WebSocket and logs events:

```javascript
const ws = new WebSocket("ws://localhost:8080/notifications");

ws.onopen = () => {
    console.log("Connected to notifications");
};

ws.onmessage = (event) => {
    const msg = JSON.parse(event.data);
    console.log(`Event: ${msg.event}`, msg.data);

    switch (msg.event) {
        case "note_created":
            // Add the new note to the UI
            break;
        case "note_updated":
            // Update the note in the UI
            break;
        case "note_deleted":
            // Remove the note from the UI
            break;
    }
};

ws.onclose = () => {
    console.log("Disconnected from notifications");
    // Reconnect after a delay
    setTimeout(() => location.reload(), 3000);
};
```

### Using pub/sub in gen_servers

Pub/sub is not limited to WebSocket handlers. Any Erlang process can join a channel. This is useful for background workers that need to react to events:

```erlang
-module(my_first_nova_note_indexer).
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_info/2, handle_cast/2, handle_call/3]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    nova_pubsub:join(notes),
    {ok, #{}}.

handle_info({nova_pubsub, notes, _Sender, "note_created", Note}, State) ->
    logger:info("Indexing new note: ~p", [maps:get(title, Note)]),
    %% Do some indexing work here
    {noreply, State};
handle_info({nova_pubsub, notes, _Sender, "note_deleted", #{id := Id}}, State) ->
    logger:info("Removing note ~p from index", [Id]),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_call(_Req, _From, State) ->
    {reply, ok, State}.
```

### Distributed pub/sub

One of the powerful things about `nova_pubsub` is that it works across Erlang nodes. If you have multiple instances of your application connected in a cluster, `broadcast/3` sends the message to all members on all nodes.

For local-only messaging, use `local_broadcast/3`:

```erlang
%% Only notifies processes on this node
nova_pubsub:local_broadcast(notes, "note_created", Note).
```

This is useful when you have node-specific side effects like clearing a local cache.

### Channels and topics

You can organize your pub/sub usage with different channels for different domains:

```erlang
%% Different channels for different concerns
nova_pubsub:join(notes).
nova_pubsub:join(users).
nova_pubsub:join(system).

%% Topics within channels for filtering
nova_pubsub:broadcast(notes, "created", Note).
nova_pubsub:broadcast(notes, "deleted", #{id => Id}).
nova_pubsub:broadcast(users, "logged_in", #{username => User}).
nova_pubsub:broadcast(system, "deploy", #{version => <<"1.2.0">>}).
```

Processes can join multiple channels and pattern match on the channel and topic in their message handlers.

### Summary

Nova's pub/sub system gives you real-time messaging with very little code:

- `join/1` and `leave/1` to manage channel membership
- `broadcast/3` to send messages to all members
- Messages arrive as regular Erlang messages
- Works across distributed Erlang nodes automatically
- No external dependencies, it is built on OTP's `pg` module

Combined with WebSockets, this gives you everything you need for real-time features like live notifications, chat, collaborative editing, and more.

### Series wrap-up

This is the last article in the Nova series. We have covered:

1. **Introduction and prerequisites** - What Nova is and how to set up your environment
2. **Getting started** - Creating and running your first Nova app
3. **Routing** - How requests are matched to controllers
4. **Plugins** - Middleware for request processing
5. **Views** - ErlyDTL templates for HTML rendering
6. **Authentication** - Security modules for protecting routes
7. **Sub-applications** - Composing multiple Nova apps together
8. **JSON APIs** - Building REST endpoints
9. **WebSockets and handlers** - Real-time communication
10. **Database integration** - Persistence with PostgreSQL
11. **Testing** - EUnit and Common Test for Nova apps
12. **Full CRUD application** - Putting it all together
13. **Deployment** - OTP releases and production setup
14. **Pub/Sub** - Real-time messaging

Nova is still a growing framework and there is more to explore. Check out the [official documentation](https://hexdocs.pm/nova/quick-start.html) and the [GitHub repository](https://github.com/novaframework/nova) for the latest updates.
