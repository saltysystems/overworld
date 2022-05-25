gen_zone Documentation
========================

What is `gen_zone`? 
-------------------

`gen_zone` is an OTP behavior that wraps around
[`gen_server`](https://www.erlang.org/doc/man/gen_server.html) to implement
a few primitives that are used by tick-based game servers. 

This module provides a number of convenience features for implementors of the
behaviour:
 * Automatic tracking of players as they join and depart
 * Client agnostic with data (de)serialization handled automatically for both
   locally attached clients (Erlang processes such as NPCs) and network clients
 * Convenient reply format - send messages to single players, multiple players,
   or broadcast zone-wide.

Callbacks
---------

Any server implementing the `gen_zone` behaviour needs to implement the
following callbacks:
|    Callback     | Description | 
| --------------- | ----------- |
| `handle_join`   | A user session connects to this zone | 
| `handle_part`   | A user session disconnects from this zone |
| `handle_action` | A user sends an action message |
| `handle_tick`   | The server has updated the global state for the next tick |

`gen_zone` response types
--------------------------
Servers that implement the `gen_zone` callback functions will have a few
different response options.

|  Response       | Description | 
| --------------- | ----------- |
| `{'@zone', Msg}` | Send a zone-wide message *Msg* to all connected clients |
| `{'@', PlayerID, Msg} | Send a message, *Msg*, to player *PlayerID*. Also accepts a list of Player IDs |
| `noreply` | Send no reply to any connected player |

The `gen_zone` gen_server keeps an internal record of connected players and
will add/remove players accordingly.

Callback implementation recommendations
------------------------------

### `handle_join`
#### Check authentication
For the handle join request, you may want to first check if the player is
authenticated with a Saline session. For example, if you need to load some
player information from a database you would probably want to first ensure that
the player has actually authenticated with the server. On the other hand, you
may want players to be able to join right away, and save authentication steps
for later - either is possible.

To check for authentication:
```
handle_join(Msg, Session, State) ->
    Authenticated = saline_session:get_authenticated(Session)
    case Authenticated of
        true ->
            % proceed to some next step
        false ->
            % dont reply
    end,
    {ok, noreply, Session, State}.
```
