Start the script server

```erlang
goblet_script_srv:start(1). % Start script server w/ ID 1
% Create a new ship component and print the resulting component ID
GDFun2 = "print(goblet.ship_component_new(\"Engine MK 1\",1,\"engine\"))".
goblet_script_srv:do(GDFun2, 1). % execute the function
% At this point, the script server is still running - anything new that we
% request will be added to the output buffer

% Ask for the name of the object we just pushed into the DB
GDFun3 = "print(goblet.ship_component_name(1))".
goblet_script_srv:do(GDFun3, 1).
```
