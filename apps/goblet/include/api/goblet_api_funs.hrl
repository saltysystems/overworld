-define(API_FUNS, 
   [
    {"goblet.square", fun([X]) -> X * X end},
    {"goblet.ship_component_new", fun([Name, Index, Type]) -> goblet_ship_component:new(Name, Index, list_to_atom(Type)) end},
    {"goblet.ship_component_delete", fun([ID]) -> goblet_ship_component:delete(ID) end},
    {"goblet.ship_component_wang_index", fun([ID]) -> goblet_ship_component:wang_index(ID) end},
    {"goblet.ship_component_attributes", fun([ID]) -> goblet_ship_component:attributes(ID) end},
    {"goblet.ship_component_name", fun([ID]) -> goblet_ship_component:name(ID) end}
   ]
 ).
