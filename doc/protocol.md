Gremlin Protocol Docs
===============

Gremlin Packet Structure
-----------------
A saline message is constructed as such:

```
    Framing (2-14 bytes)       OpCode (2 bytes)     Payload (N bytes)
 |--------------------------|------------------|--------------------------|
     WebSocket Data                Gremlin-specific Data
```

WebSocket is used as the transport mechanism thanks to easy firewall negotation, etc. Packets should probably not be much larger than 1000 bytes to avoid fragmentation on the wire. 


Gremlin RPC structure
------------------------


| Key         | Type            | Description                                  |
| ---         | -----            | ------------------------------------------   |
| `opcode`      | `16#0000..16#FFFFF` | A 2-byte integer prefix indicating the type of message to be processed |
| `c2s_call`    | `atom()`           | Name of a client-to-server calling function in the generated client library |
| `c2s_handler` | `mfa()`            | A module and function to handle client. The function must either be arity 1 (handles messages from clients without established sessions) or arity 2 (message + client session state)  |
| `s2c_call`    | `atom()`           | Name of a server-to-client calling function, which will generate a signal of the same name in the client library  |
| `encoder`     | `atom()`           | Name of the module that will marshall/unmarshall data with [GPB](https://github.com/tomas-abrahamsson/gpb) | 


OpCodes `0x0` through `0x1000` are soft reserved for Gremlin Core messages, while `0x1001` and above are free for applications to use. Gremlin does not throw an error if a OpCode is reused.

Defining a new RPC
-----------------
To define a new message, simply write a module using the behaviour `saline_rpc` and implementing the required callbacks, `rpc_info/0` which should return a list of maps with keys as above.


Messages in Gremlin can be synchronous or asynchronous. Synchronous messages initiated by the client are defined by 2 keys: 
 - `c2s_call`, which will generate an function in the client library with appropriate arguments
 - `c2s_handler`, which corresponds to some module and function to process the message. Note that your handler's function must be either arity 1 (to process messages without an established session, see `account_new` for an example)  or arity 2.


Generating the client library
---------------------

Gremlin can automatically generate a client library in GDScript usable by Godot v3.x.

This library should be dropped into your scripts folder, along with your protobuf file. You will need to install the [Godobuf](https://github.com/oniksan/godobuf) plugin in order to generate code to marshall/unmarshall data from Protobuf from/to your Godot client. You'll want to autoload it with some name, I suggest `NetworkClient`. 

Once Erlang is running, you can invoke the following to write out a library:

```
1> saline_binding:write().
```

The library file will live in `apps/saline_core/static/libsaline.gd`. It's up to you to distribute the file. 



Examples
--------------

### A simple client message with no response from server.

First we write a protobuf file describing the module:

``` 
syntax = "proto2";

package my_game;

message hello {
    required string msg = 1;
}
```

By default, GPB will generate an Erlang file named `<your>_<module>_pb.erl`. 

And then write a module that can generate the client call library, and handle the message:

```
-module(my_module).

-behaviour(saline_rpc).

% Required callback for Gremlin
-export([rpc_info/0]).

% A trivial example where the client can ask the server for buffs
-export([hello/2]).

-define(HELLO, 16#2000). % This makes the rpc info a bit more readable

-spec rpc_info() -> saline_rpc:callbacks().
rpc_info() -> 
    [
        #{ 
            opcode => ?HELLO,
            c2s_call => send_hello, % autogenerates a function of the same name
            c2s_handler => {?MODULE, hello, 2},
            encoder => my_game
         }
    ].
    

-spec client_hello(binary(), saline_session:session()) ->
client_hello(Data, Session) ->
    Decoded = my_game_pb:decode_msg(Data, hello)
    Msg = maps:get(Decoded, msg),
    logger:notice("Client sends: ~p", [Msg]),
    {ok, Session}.
  
```

A client using `libsaline.gd` will be able to simply call

```
NetworkClient.send_hello("Hello world!")
```
which should be processed by your `my_module` handler after it is routed through Gremlin Core.


Built-in messages
--------------------
Gremlin has a few built in messages that you can reuse in your own modules.

### gen_response
The general response message `gen_response` encodes an an enum of either 0=OK or 1=ERROR with optional string to describe the error.

#### Example
Here's an example of a handler that assumes you have a function that can decode the message called `decode` and a function with conditional results called `blahblah`. `gen_response` is invoked here with `ok` returning nothing other than acknowledgement back to the client, and `error` with a rather unhelpful error message.
```
myfun(Msg, Session) -> 
    DecodedMsg = decode(Msg),
    Reply = 
    case blahblah(DecodedMsg, Session) of 
        foo -> 
            saline_protocol:response(ok);
        bar ->
            saline_protocol:response(error, "Blahblah failed somehow")
    end,
    [<<?YOUR_OPCODE:16>>, Reply].
```

