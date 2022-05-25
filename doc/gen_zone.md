gen_zone Documentation
========================

What is `gen_zone`? 
-------------------

`gen_zone` is an OTP behavior that wraps around
[`gen_server`](https://www.erlang.org/doc/man/gen_server.html) to implement
a few primitives that are used by tick-based game servers. 

Any server implementing the `gen_zone` behaviour needs to implement the
following callbacks:
|    Callback     | Description | 
| --------------- | ----------- |
| `handle_join`   | A user session connects to this zone | 
| `handle_part`   | A user session disconnects from this zone |
| `handle_action` | A user sends an action message |
| `handle_tick`   | The server has updated the global state for the next tick |

Implementation recommendations
------------------------------

# `handle_join`
## Check authentication
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
