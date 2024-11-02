Overworld Zone Behaviour
========================

What is `ow_zone`? 
-------------------

`ow_zone` is an OTP behavior that wraps around
[`gen_server`](https://www.erlang.org/doc/man/gen_server.html) to implement
a few primitives that are used by tick-based game servers. 

This module provides a number of convenience features for implementors of the
behaviour:
 * Automatic tracking of players as they join and depart
 * Client agnostic with data (de)serialization handled automatically for both
   locally attached clients (Erlang processes such as NPCs) and network clients
 * Convenient reply format - send messages to single players, multiple players,
   or broadcast zone-wide.

Initial ConfigMap
---------

When creating a server with the ow_zone behaviour, you can specify a ConfigMap
to configure various aspects of ow_zone. ow_zone understands the following keys:
```
#{ 
    require_auth :: boolean(),
    tick_rate :: pos_integer()
}
```

where:
|    Callback     | Description | 
| --------------- | ----------- |
| `require_auth`    | Check whether or not a client session has authenticated with Overworld (default `false`) |
| `tick_rate`       | Set the rate at which the server processes a tick, in milliseconds (default `30`) |


Callbacks
---------

Any server implementing the `ow_zone` behaviour needs to implement the
following callbacks:
|    Callback     | Description | 
| --------------- | ----------- |
| `handle_join`   | A user session connects to this zone | 
| `handle_part`   | A user session disconnects from this zone |
| `handle_rpc`    | A user sends an action message |
| `handle_tick`   | The server has updated the global state for the next tick |

Optional callbacks:
|    Callback     | Description | 
| --------------- | ----------- |
| `handle_status` | Arbitrary term containing stat information | 
| `rpc_info`      | A list of Overworld RPCs (see the [Protocol documentation](../architecture/protocol.md))| 


Optional Callbacks
---------

You can provide arbitrary stats about your ow_zone via the status/0 callback. 


`ow_zone` reply format
--------------------------
Servers that implement the `ow_zone` callback functions will have a few
different response options.

|  Response       | Description | 
| --------------- | ----------- |
| `{'@zone', Msg}` | Send a zone-wide message *Msg* to all connected clients |
| `{'@', [PlayerID, ...], Msg}` | Send a message, *Msg*, to players specified by *PlayerID*. |
| `noreply` | Send no reply to any connected player |

The `ow_zone` gen_server keeps an internal record of connected players and
will add/remove players accordingly. Each callback handler for `ow_zone` will
take "Players" as an argument, where the Overworld will pass the best 
knowledge of connected players at that time to the handler. 


Implementing Handlers
------------

To implement an Overworld Zone handler, you'll need to construct a tuple with 3 terms at the end of every function.
```
                              ok
                        {ok, SessionUpdate}
    { Reply,     {ok, SessionUpdate, PlayerUpdate},       GameState }.
       |                       |                              |
       |                       |                              |
  A reply in the               |                      Your current, and   
  form of the reply            |                      possibly updated game
  format as seen in            |                      loop state at the 
  the previous                 |                      end of the handler.
  section.                     |
                               |
                  Any updates to the player's session
                  (SessionUpdate) plus any updates to
                  the Player object in the Player
                  Registry (PlayerUpdate). Updates are
                  optional.
```
