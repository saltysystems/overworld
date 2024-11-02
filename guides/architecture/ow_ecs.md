Overworld Entity Component System (ECS)
=======================================

Overworld has a built-in entity component system (ECS) for managing
objects. 


Starting ECS
------------------
In your Overworld server, you can define any number of ECS "worlds".

To start an ECS world:

```erlang
ow_ecs:start_link(Name)
```

where `Name` is some Erlang term as used by `gproc`. 


Adding components 
-----------------
Components and initial data can be added via `add_component/4`. 

An entity will be initialized whenever the first component is added to
it.

```erlang
EntityID = erlang:unique_integer(),
ow_ecs:add_component(ComponentName, ComponentData, EntityID, WorldName)
```


Adding systems
--------------
You can define systems with priorities that will be called in order
whenever `ow_ecs:proc/1` is called. Either in `fun()` format or `mfa()`
format:
```erlang
Fun = fun(Table) -> do_something_with_the_table(Table) end,
ow_ecs:add_system(Fun, WorldName)
```
or
```erlang
ow_ecs:add_system({Module, Fun, 1}, WorldName)
```


Querying components
-------------------
When a system is triggered by a call to `proc/1`, you can get a list of
matching compnents via `ow_ecs:match_component`:

```erlang
handle_system(Table) ->
  Matches = ow_ecs:match_component("MyComponent", Table).
```

where `Matches` will be a list of all entities that match the component
asked for, plus the data associated with that component.



(TODO) Adding listeners
-----------------------
It is also possible to add processes that listen for changes to
components and dispatch messages to another process.

```erlang
ow_ecs:register_listener(Pid, WorldName)
```
