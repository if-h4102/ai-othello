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

% test where ia must not choose the highest amount of case to swap (case before corner).
testNotCorner(bestMove) :-
    Board = [[ _, _, _, _, _, _, _, _, _, _],
             [ _, _, _, _, _, _, _, _, _, _],
             [ _, _, _, _, _, _, _, _, _, _],
             [ _, _, _, _, _, _, 1, _, _, _],
             [ _, _, _, _, _, 1, _, _, _, _],
             [ _, _, _, _,-1, _, _, _, _, _],
             [ _, _, _, 1, _, _, _, _, _, _],
             [ _, _, _, _, _, _, _, _, _, _],
             [ _, _, _, _, _, _, _, _, _, _],
             [ _, _, _, _, _, _, _, _, _, _]],
    ai:bestMove(Board, 1, 2, X, Y),
    X==7,
    Y==3.


% test where ia must not choose the highest amount of case to swap (corner is possible).
testCorner(bestMove) :-
    Board = [[ _, _, _, _, _, _, _, _, _, _],
             [ _, _, _, _, _, _, _, _, _, _],
             [ _, _, _, _, _, _, _, 1, _, _],
             [ _, _, _, _, _, _,-1, _, _, _],
             [ _, _, _, _, _, _, 1, _, _, _],
             [ _, _, _, _, _, _, 1, _, _, _],
             [ _, _, _, _, _, _, _, _, _, _],
             [ _, _, _, _, _, _, _, _, _, _],
             [ _, _, _, _, _, _, _, _, _, _],
             [ _, _, _, _, _, _, _, _, _, _]],
    ai:bestMove(Board, 1, 2, X, Y),
    X==1,
    Y==8.


:- end_tests(basic_ai).
