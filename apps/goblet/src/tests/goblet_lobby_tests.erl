-module(goblet_lobby_tests).
-include_lib("eunit/include/eunit.hrl").

-include("apps/goblet/include/goblet_database.hrl").

get_matches_empty_test() ->
    Matches = goblet_lobby:get_matches(),
    ?assertEqual([], Matches).

add_remove_match_test() ->
    PlayersMax = 10,
    Mode = blitz,
    {ok, ConfirmedMatch} = goblet_lobby:create_match(Mode, PlayersMax),
    ?assertEqual([], ConfirmedMatch#goblet_match.players),
    ?assertEqual(PlayersMax, ConfirmedMatch#goblet_match.players_max),
    ?assertEqual(Mode, ConfirmedMatch#goblet_match.mode),
    ?assertEqual('CREATING', ConfirmedMatch#goblet_match.state),
    
    Matches1 = goblet_lobby:get_matches(),
    ?assertEqual([ConfirmedMatch], Matches1),

    Result = goblet_lobby:delete_match(ConfirmedMatch#goblet_match.id),
    ?assertEqual(ok, Result),
    Matches2 = goblet_lobby:get_matches(),
    ?assertEqual([], Matches2).
