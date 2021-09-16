-define(API_FUNS, 
   [
    {"goblet.square", fun([X]) -> X * X end},
    {"goblet.ship_component_new", fun([Name, Index, Type, Appearance, Attributes]) -> goblet_ship_component:new(Name, Index, Type, Appearance, Attributes) end},
    {"goblet.ship_component_delete", fun([ID]) -> goblet_ship_component:delete(ID) end},
    {"goblet.ship_component_wang_index", fun([ID]) -> goblet_ship_component:wang_index(ID) end},
    {"goblet.ship_component_attributes", fun([ID]) -> goblet_ship_component:attributes(ID) end},
    {"goblet.ship_component_name", fun([ID]) -> goblet_ship_component:name(ID) end},
    {"goblet.ship_component_info", fun([ID]) -> goblet_ship_component:info(ID) end}
   ]
 ).
