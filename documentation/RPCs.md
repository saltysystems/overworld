Gremlin Protocol Docs
===============

Gremlin Packet Structure
-----------------
A gremlin message is constructed as such:

```
    Framing (2-14 bytes)       OpCode (2 bytes)     Payload (N bytes)
 [--------------------------(------------------|--------------------------)]
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
To define a new message, simply write a module using the behaviour `gremlin_rpc` and implementing the required callbacks, `rpc_info/0` which should return a list of maps with keys as above.


Messages in Gremlin can be synchronous or asynchronous. Synchronous messages initiated by the client are defined by 2 keys: 
 - `c2s_call`, which will generate an function in the client library with appropriate arguments
 - `c2s_handler`, which corresponds to some module and function to process the message. Note that your handler's function must be either arity 1 (to process messages without an established session, see `account_new` for an example)  or arity 2.


