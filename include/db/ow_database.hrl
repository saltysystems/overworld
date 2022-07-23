% Database directory. Lives in /tmp by default, probably will get nuked upon
% reboot!
-define(DATABASE_DIR, "/tmp/ow_db"). % THIS MUST BE CHANGED IN SYS.CONFIG AS
                                      % WELL

% Records representing the database tables in Mnesia
-record(ow_account,{email, id, hash, salt, first_login, last_login, player_ids}).
%-record(ow_player,{name, id, flags, inventory, shipgrid}).
%-record(ow_mob,{name, id, flags, inventory, shipgrid, script}).
%-record(ow_item, {name, id, ap, action, target_type, target_damage, target_health, status_effect, flags, price}).
%-record(ow_ship_component, {id, name, wang_index, type, appearance, attributes}).
