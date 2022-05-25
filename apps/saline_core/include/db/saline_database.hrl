% Database directory. Lives in /tmp by default, probably will get nuked upon
% reboot!
-define(DATABASE_DIR, "/tmp/saline"). % THIS MUST BE CHANGED IN SYS.CONFIG AS
                                      % WELL

% Records representing the database tables in Mnesia
-record(saline_account,{email, id, hash, salt, first_login, last_login, player_ids}).
%-record(saline_player,{name, id, flags, inventory, shipgrid}).
%-record(saline_mob,{name, id, flags, inventory, shipgrid, script}).
%-record(saline_item, {name, id, ap, action, target_type, target_damage, target_health, status_effect, flags, price}).
%-record(saline_ship_component, {id, name, wang_index, type, appearance, attributes}).
