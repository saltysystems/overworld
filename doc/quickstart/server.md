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

## Implementing the chat zone 

The Chat Zone is the core component of our chat application, handling all client interactions and message routing. This section will guide you through implementing the `chat_zone` module, explaining each part in detail.

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

This function initializes the zone's state. In this simple chat application, we start with an empty map for both internal State and the `ow_zone` Config. You can add any initial state or configuration here as needed. For example, you might want to change the tick rate via `Config = #{ tick_ms => 100 }` for a 100ms tick rate. You can find further documentation [here](../ow_zone.md). 

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
