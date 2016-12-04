:- module('test_basic_ai', []).
:- use_module(library(plunit)).
:- use_module('../src/ai/basic-ai', []).

% To run this test case
test_basic_ai :-
	run_tests([basic_ai]).

% Define the test case
:- begin_tests(basic_ai).

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
  findall(X, basic_ai:possibleMoves(Board, 1, X), [[[4, 6], [3, 5]], [[3, 5], [4, 6]]]).
  

:- end_tests(basic_ai).
