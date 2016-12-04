:- module('test_basic_ai', []).
:- use_module(library(plunit)).
:- use_module('../src/ai/ai', []).

% To run this test case
test_basic_ai :-
	run_tests([basic_ai]).

% Define the test case
:- begin_tests(basic_ai).  

test(bestMove) :-
    Board = [[ _, _, _, _, _, _, _, _, _, _],
             [ _, _, _, _, _, _, _, _, _, _],
             [ _, _, _, _, _, _, _, _, _, _],
             [ _, _, _, _, _, _, _, _, _, _],
             [ _, _, _, _, 1,-1,-1, _, _, _],
             [ _, _, _, _, _, 1, 1, _, _, _],
             [ _, _, _, _, _, _, _, _, _, _],
             [ _, _, _, _, _, _, _, _, _, _],
             [ _, _, _, _, _, _, _, _, _, _],
             [ _, _, _, _, _, _, _, _, _, _]],
    ai:bestMove(Board, 1, 2, X, Y),
    X == 4,
    Y == 7.

:- end_tests(basic_ai).
