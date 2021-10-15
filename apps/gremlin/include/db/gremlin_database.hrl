% Database directory. Lives in /tmp by default, probably will get nuked upon
% reboot!
-define(DATABASE_DIR, "/tmp/gremlin"). % THIS MUST BE CHANGED IN SYS.CONFIG AS
                                      % WELL

% Records representing the database tables in Mnesia
-record(gremlin_account,{email, id, hash, salt, first_login, last_login, player_ids}).
%-record(gremlin_player,{name, id, flags, inventory, shipgrid}).
%-record(gremlin_mob,{name, id, flags, inventory, shipgrid, script}).
%-record(gremlin_item, {name, id, ap, action, target_type, target_damage, target_health, status_effect, flags, price}).
%-record(gremlin_ship_component, {id, name, wang_index, type, appearance, attributes}).
