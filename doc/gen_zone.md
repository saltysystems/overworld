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
   
Initial ConfigMap
---------

When creating a server with the gen_zone behaviour, you can specify a ConfigMap
to configure various aspects of gen_zone. gen_zone understands the following keys:
```
#{ 
    require_auth :: boolean(),
    tick_rate :: pos_integer()
},

where:
|    Callback     | Description | 
| --------------- | ----------- |
| require_auth    | Check whether or not a client session has authenticated with Saline (defualt false) |
| tick_rate       | Set the rate at which the server processes a tick, in milliseconds (default 30) |


Callbacks
---------

Any server implementing the `gen_zone` behaviour needs to implement the
following callbacks:
|    Callback     | Description | 
| --------------- | ----------- |
| `handle_join`   | A user session connects to this zone | 
| `handle_part`   | A user session disconnects from this zone |
| `handle_rpc` | A user sends an action message |
| `handle_tick`   | The server has updated the global state for the next tick |

Optional callbacks:
|    Callback     | Description | 
| --------------- | ----------- |
| `handle_status` | Arbitrary term containing stat information | 
| `rpc_info`      | A list of 


Optional Callbacks
---------

You can provide arbitrary stats about your gen_zone via the status/0 callback. 



`gen_zone` response types
--------------------------
Servers that implement the `gen_zone` callback functions will have a few
different response options.

|  Response       | Description | 
| --------------- | ----------- |
| `{'@zone', Msg}` | Send a zone-wide message *Msg* to all connected clients |
| `{'@', PlayerID, Msg}` | Send a message, *Msg*, to player *PlayerID*. Also accepts a list of Player IDs |
| `noreply` | Send no reply to any connected player |

The `gen_zone` gen_server keeps an internal record of connected players and
will add/remove players accordingly. noreply, Session, State}.
```
