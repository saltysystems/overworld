-module(goblet_instance_server_tests).
-include_lib("eunit/include/eunit.hrl").

-record(action, {ap, name, from, target}).

normalize_actions_test() -> 
	Actions = [
		#action{name=shoot, ap=2, from="player1", target="player2"},
		#action{name=move, ap=1, from="player1", target={0,0}},
		#action{name=laz0r, ap=6, from="player2", target="player1"}
	],
	% Convert AP to Phases
	MP = goblet_instance_server:normalize_actions(Actions),
	Last = lists:last(MP),
	?assertEqual(9, Last#action.ap).
	%?assertEqual(9, Last#action.mp).

phases_test() -> 
	P1_Actions = [
		#action{name=shoot, ap=2, from="player1", target="player2"},
		#action{name=move, ap=1, from="player1", target={0,0}},
		#action{name=laz0r, ap=6, from="player1", target="player2"}
	],
	P2_Actions = [
		#action{name=move, ap=1, from="player2", target={1,1}},
		#action{name=shoot, ap=2, from="player2", target="player1"},
		#action{name=shield, ap=3, from="player2", target="player1"},
		#action{name=missile, ap=3, from="player2", target="player1"}
	],
	% Convert AP to Phases
	MP = goblet_instance_server:normalize_actions(P1_Actions),
	MP2 = goblet_instance_server:normalize_actions(P2_Actions),
	Collected = MP ++ MP2,
	% Group the phases
	goblet_instance_server:phases(Collected).
