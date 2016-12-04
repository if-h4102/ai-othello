:- module('test_random_ai', []).
:- use_module(library(plunit)).
:- use_module('../src/ai/random-ai', []).

% To run this test case
test_random_ai :-
	run_tests([random_ai]).

% Define the test case
:- begin_tests(random_ai).

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
  random_ai:canBePlayed(Board, 1, [4, 6]).

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
  findall(X, random_ai:possibleMoves(Board, 1, X), [[[4, 6], [3, 5]], [[3, 5], [4, 6]]]).
  

:- end_tests(random_ai).
