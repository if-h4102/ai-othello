:- module('test_gameloop', [test_gameloop/0]).
:- use_module(library(plunit)).
:- use_module('../src/gameloop/gameloop', []).
:- use_module('../src/game/end-of-game', []).

% To run this test case
test_gameloop :-
	run_tests([gameloop]).

% Define the test case
:- begin_tests(gameloop).

test(determine_first_player) :-
	gameloop:determineFirstPlayer(FirstPlayer),
        (FirstPlayer == -1 ; FirstPlayer == 1),
	!.

test(determine_player_type1) :-
	gameloop:determinePlayerType(-1,2,-1,-1).

test(determine_player_type2) :-
	gameloop:determinePlayerType(0,1,1,1).

test(check_given_coordinates) :-
	Board = [[ _, _, _, _, _, _, _, _, _, _],
		 [ _, _, _, _, _, _, _, _, _, _],
		 [ _, _, _, _, _, _, _, _, _, _],
		 [ _, _, _, _, _, _, _, _, _, _],
		 [ _, _, _, _, 1,-1, _, _, _, _],
		 [ _, _, _, _, 1, 1, _, _, _, _],
		 [ _, _, _, _, _, _, _, _, _, _],
		 [ _, _, _, _, _, _, _, _, _, _],
		 [ _, _, _, _, _, _, _, _, _, _],
		 [ _, _, _, _, _, _, _, _, _, _]],
	gameloop:checkGivenCoordinates(Board,1,X,Y,3,5),
	X == 3,
	Y == 5.


