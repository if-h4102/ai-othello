:- module('test_utils_ai', []).
:- use_module(library(plunit)).
:- use_module('../src/ai/utils_ai', []).

% To run this test case
test_utils_ai :-
	run_tests([utils_ai]).

% Define the test case
:- begin_tests(utils_ai).

test(can_be_played) :-
  Board = [[ _, _, _, _, _, _, _, _, _, _],
           [ _, _, _, _, _, _, _, _, _, _],
           [ _, _, _, _, _, _, _, _, _, _],
           [ _, _, _, _, _, _, _, _, _, _],
           [ _, _, _, _, 1,-1, _, _, _, _],
           [ _, _, _, _, _, _, _, _, _, _],
           [ _, _, _, _, _, _, _, _, _, _],
           [ _, _, _, _, _, _, _, _, _, _],
           [ _, _, _, _, _, _, _, _, _, _],
           [ _, _, _, _, _, _, _, _, _, _]],
    utils_ai:canBePlayed(Board, 1, [4, 6]),
    !.

test(possible_moves) :-
    Board = [[ _, _, _, _, _, _, _, _, _, _],
           [ _, _, _, _, _, _, _, _, _, _],
           [ _, _, _, _, _, _, _, _, _, _],
           [ _, _, _, _, _, _, _, _, _, _],
           [ _, _, _, _, 1,-1, _, _, _, _],
           [ _, _, _, _, _, 1, _, _, _, _],
           [ _, _, _, _, _, _, _, _, _, _],
           [ _, _, _, _, _, _, _, _, _, _],
           [ _, _, _, _, _, _, _, _, _, _],
           [ _, _, _, _, _, _, _, _, _, _]],
    utils_ai:possibleMoves(Board, 1, [[3, 5], [4, 6]]).

:- end_tests(utils_ai).
