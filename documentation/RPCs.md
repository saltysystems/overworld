RPCs
===============

Gremlin Packet Structure
-----------------
A gremlin message is constructed as such:

```
    Framing (2-14 bytes)       OpCode (2 bytes)     Payload (N bytes)
 [--------------------------(------------------|--------------------------)]
     WebSocket Data                Gremlin-specific Data
```

WebSocket is used as the transport mechanism thanks to easy firewall negotation, etc.






Description of Gremlin RPC structure

| Key         | Required | Types            | Description                                  |
| ---         | :------: | -----            | ------------------------------------------   |
| OpCode      |   Yes    | 16#0000..16#9999 | A 2-byte integer prefix indicating the type of message to be processed |
| c2s_call    |   No     | atom()           | Name of a client-to-server calling function in the generated client library |
| c2s_handler |   No     | mfa()            | A module and function to handle client  |
| s2c_call    |   No     | atom()           | Name of a server-to-client calling function, which will generate a signal of the same name in the client library  |
| encoder     |   No     | atom()           | Name of the module that will marshall/unmarshall data with [GPB](https://github.com/tomas-abrahamsson/gpb) | 

