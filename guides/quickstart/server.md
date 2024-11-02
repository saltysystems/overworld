# Conceptual Overview
Overworld is built around the concept of "Zones". A Zone is an Erlang process that manages a group of connected clients, handling tasks such as:

* Connected clients 
* Message serialization and deserialization as needed (Protobuf for networked clients, tuples for Erlang clients) 
* Broadcasting messages to connected clients or specific lists of clients 
* Implementing game via periodic ticks with configurable tick rate

On the client side, Overworld generates Godot code that interfaces with the Erlang server, providing an easy-to-use API for sending and receiving messages.

# Server Implementation Guide
## Prerequisites

*  [Erlang/OTP 27](https://www.erlang.org/downloads)
*  [rebar3](https://rebar3.org/)

## Creating the `chat` application
```bash
rebar3 new app chat
cd chat
```

Edit `rebar.config` to include Overworld and the Protocol Buffers plugins:
```erlang
deps, [
    {overworld, {git, "https://github.com/saltysystems/overworld.git", {branch, "master"}}}
]}.

{plugins, [
    {rebar3_gpb_plugin, "2.23.2"}
]}.

{erl_opts, [
    {i, "./_build/default/plugins/gpb/include"}
]}.

{gpb_opts, [
    {i, "priv/proto"},
    {module_name_suffix, "_pb"},
    {o_erl, "src"},
    {o_hrl, "src"},
    {strings_as_binaries, false},
    {maps, true},
    {msgs_as_maps, true},
    {mapfields_as_maps, true},
    type_specs
]}.

{provider_hooks, [
    {pre, [
        {compile, {protobuf, compile}},
        {clean, {protobuf, clean}}
    ]}
]}.
```

Run `rebar3 upgrade` to fetch dependencies.

## Implementing the chat zone module
First, let's create the chat_zone. From the top-level directory of the chat application:

```bash
touch src/chat_zone.erl
```

Now open the `chat_zone.erl` module and paste in the following. We'll go over it section-by-section afterwards. 
```erlang
-module(chat_zone).
-behaviour(ow_zone).

-export([
         start_link/0,
         stop/0,
         join/2,
         part/2,
         channel_msg/2
        ]).

-export([init/1,
         handle_join/4,
         handle_part/4,
         handle_channel_msg/4,
         handle_tick/2
        ]).

-define(SERVER, ?MODULE).

-rpc_client([sync, channel_msg]).
-rpc_server([join, part, channel_msg]).

start_link() ->
    ow_zone:start_link({local, ?SERVER}, ?MODULE, [], []).
stop() ->
    ow_zone:stop(?SERVER).
join(Msg, Who) ->
    ow_zone:join(?SERVER, Msg, Who).
part(Msg, Who) ->
    ow_zone:part(?SERVER, Msg, Who).
channel_msg(Msg, Who) ->
    ow_zone:rpc(?SERVER, channel_msg, Msg, Who).

init([]) ->
    State = #{},
    Config = #{},
    {ok, State, Config}.

handle_join(Msg, Who, _ZD, State) ->
    SessionID = ow_session:id(Who),
    Handle = maps:get(handle, Msg, "Unknown" ++ integer_to_list(SessionID)),
    logger:notice("Player ~p (~p) has joined the chat.", [Handle, Who]),
    State1 = State#{ Who => Handle },
    Handles = maps:values(State1),
    BcastMsg = {sync, #{ handles => Handles }},
    {broadcast, BcastMsg, State1}.

handle_part(_Msg, Who, _ZD, State) ->
    #{ Who := Handle } = State,
    logger:notice("Player ~p (~p) has left the chat.", [Handle, Who]),
    State1 = maps:remove(Who, State),
    Handles = maps:values(State1),
    BcastMsg = {sync, #{ handles => Handles }},
    {broadcast, BcastMsg, State1}.

handle_channel_msg(Msg, Who, _ZD, State) ->
    #{ Who := Handle } = State,
    logger:notice("<~p>: ~p", [Handle, Msg]),
    Msg1 = Msg#{ handle => Handle },
    ow_zone:broadcast(?SERVER, {channel_msg, Msg1}),
    {noreply, State}.
handle_tick(_ZoneData, State) ->
    {noreply, State}.
```

Next we'll explain each component in detail. 

### Module Structure

Let's start by breaking down the structure of the `chat_zone` module. Here are the top-level annotations for the module:

```erlang
-module(chat_zone).
-behaviour(ow_zone).

% API exports
-export([
    start_link/0,
    stop/0,
    join/2,
    part/2,
    channel_msg/2
]).

% Callback exports
-export([
    init/1,
    handle_join/4,
    handle_part/4,
    handle_channel_msg/4,
    handle_tick/2
]).

% RPC annotations
-rpc_client([sync, channel_msg]).
-rpc_server([join, part, channel_msg]).
```

#### Behavior Declaration

The `-behaviour(ow_zone).` line declares that this module implements the `ow_zone` behavior. This is similar to implementing `gen_server`, but tailored for tick-based games in Overworld. 

#### API Functions

The first export list defines the public API for the chat zone. These functions will be called by other parts of your application to interact with the chat zone.

#### Callback Functions

The second export list defines the callback functions required by the `ow_zone` behavior. These functions will be called by Overworld in response to various events.

#### RPC Annotations

The `-rpc_client` and `-rpc_server` annotations are crucial for Overworld's code generation features:

- `-rpc_client([sync, channel_msg]).` indicates that the server can send `sync` and `channel_msg` messages to clients.
- `-rpc_server([join, part, channel_msg]).` indicates that clients can send `join`, `part`, and `channel_msg` messages to the server.

These annotations will generate appropriate Godot signals and functions in the client library. For example, the `sync` function will be available in Godot as a signal `server_sync` emitted in the `Overworld` module. Likewise, `join` will be available to call via `Overworld.join(handle: String)`. 

###  Implementing API Functions

Now, let's implement the API functions:

```erlang
-define(SERVER, ?MODULE).

start_link() ->
    ow_zone:start_link({local, ?SERVER}, ?MODULE, [], []).

stop() ->
    ow_zone:stop(?SERVER).

join(Msg, Who) ->
    ow_zone:join(?SERVER, Msg, Who).

part(Msg, Who) ->
    ow_zone:part(?SERVER, Msg, Who).

channel_msg(Msg, Who) ->
    ow_zone:rpc(?SERVER, channel_msg, Msg, Who).
```

These functions mostly delegate to the `ow_zone` module, which handles the underlying mechanics of the zone behavior. The `?SERVER` macro is used to refer to the module name, which is a common pattern in Erlang for naming registered processes.

### Implementing Callback Functions

Next, we'll implement the callback functions required by the `ow_zone` behavior:

#### init/1

```erlang
init([]) ->
    State = #{},
    Config = #{},
    {ok, State, Config}.
```

This function initializes the zone's state. In this simple chat application, we start with an empty map for both internal State and the `ow_zone` Config. You can add any initial state or configuration here as needed. For example, you might want to change the tick rate via `Config = #{ tick_ms => 100 }` for a 100ms tick rate. You can find further documentation [here](../architecture/ow_zone.md). 

#### handle_join/4

```erlang
handle_join(Msg, Who, _ZD, State) ->
    SessionID = ow_session:id(Who),
    Handle = maps:get(handle, Msg, "Unknown" ++ integer_to_list(SessionID)),
    logger:notice("Player ~p (~p) has joined the chat.", [Handle, Who]),
    State1 = State#{ Who => Handle },
    Handles = maps:values(State1),
    BcastMsg = {sync, #{ handles => Handles }},
    {broadcast, BcastMsg, State1}.
```

This function handles a client joining the chat. It:
1. Extracts the client's handle from the join message or generates one.
2. Logs the join event.
3. Updates the state with the new client.
4. Prepares a broadcast message with the updated list of handles.
5. Returns a tuple instructing Overworld to broadcast the message and update the state.

#### handle_part/4

```erlang
handle_part(_Msg, Who, _ZD, State) ->
    #{ Who := Handle } = State,
    logger:notice("Player ~p (~p) has left the chat.", [Handle, Who]),
    State1 = maps:remove(Who, State),
    Handles = maps:values(State1),
    BcastMsg = {sync, #{ handles => Handles }},
    {broadcast, BcastMsg, State1}.
```

This function is similar to `handle_join/4`, but handles a client leaving the chat. It removes the client from the state and broadcasts an updated list of handles.

#### handle_channel_msg/4

```erlang
handle_channel_msg(Msg, Who, _ZD, State) ->
    #{ Who := Handle } = State,
    logger:notice("<~p>: ~p", [Handle, Msg]),
    Msg1 = Msg#{ handle => Handle },
    ow_zone:broadcast(?SERVER, {channel_msg, Msg1}),
    {noreply, State}.
```

This function handles chat messages from clients. It logs the message, adds the sender's handle to the message, and broadcasts it to all clients.

#### handle_tick/2

```erlang
handle_tick(_ZoneData, State) ->
    {noreply, State}.
```

This function is called periodically by Overworld. In this simple chat application, we don't need to do anything on each tick, so we just return the unchanged state.

## Chat Application Protocol Buffer Schema

### Overview

For our chat application, we use Protocol Buffers to define the structure of messages exchanged between the Erlang server and Godot clients. This schema defines four types of messages: join, part, channel_msg, and sync.

### Creating the Schema File

Create a new file `chat.proto` in the `priv/proto` directory of your Erlang application:

```bash
mkdir -p priv/proto
touch priv/proto/chat.proto
```

### The Schema Definition

Open `chat.proto` and add the following content:

```protobuf
syntax = "proto2";

package chat;

message chat {
    oneof msg {
        join        join        = 1;
        part        part        = 2;
        channel_msg channel_msg = 3;
        sync        sync        = 4;
    }
}

message join {
    optional string handle = 1;
}

message part {
}

message channel_msg {
    optional string handle = 1;
    optional string text = 2;
}

message sync {
    repeated string handles = 1;
}
```

### Explanation of Message Types

#### Main chat Message

The `chat` message is the wrapper for all other message types. It uses a `oneof` field, which means each `chat` message will contain exactly one of the submessage types.

```protobuf
message chat {
    oneof msg {
        join        join        = 1;
        part        part        = 2;
        channel_msg channel_msg = 3;
        sync        sync        = 4;
    }
}
```

#### Join Message

Used when a client joins the chat.

```protobuf
message join {
    optional string handle = 1;
}
```

- `handle`: The user's chosen display name. It's optional, allowing the server to assign a default if none is provided.

#### Part Message

Used when a client leaves the chat.

```protobuf
message part {
}
```

This message is empty because no additional information is needed when a user leaves.

#### Channel Message

Used for sending chat messages.

```protobuf
message channel_msg {
    optional string handle = 1;
    optional string text = 2;
}
```

- `handle`: The sender's display name.
- `text`: The content of the message.

#### Sync Message

Used to update clients with the current list of users in the chat.

```protobuf
message sync {
    repeated string handles = 1;
}
```

- `handles`: A list of all current user handles in the chat.

### Usage in the Chat Application

- When a user joins, the client sends a `join` message with their chosen handle.
- The server responds with a `sync` message to all clients, updating the user list.
- Users send chat messages using the `channel_msg` type.
- When a user leaves, the client sends a `part` message, and the server again sends a `sync` to all remaining clients.

This schema provides a simple yet flexible structure for our chat application, allowing for easy extension if we need to add more features in the future.
