:- module('test_basic_ai', []).
:- use_module(library(plunit)).
:- use_module('../src/ai/ai', []).
:- use_module('../src/ai/basic-ai', []).

% To run this test case
test_basic_ai :-
	run_tests([basic_ai]).

% Define the test case
:- begin_tests(basic_ai).  

test(bestMove) :-
    Board = [[ _, _, _, _, _, _, _, _, _, _],
             [ _, _, _, _, _, _, _, _, _, _],
             [ _, _, _, _, _, _, _, _, _, _],
             [ _, _, _, _, 1, 1, _, _, _, _],
             [ _, _, _, 1,-1,-1, _, _, _, _],
             [ _, _, _, _, 1, 1, _, _, _, _],
             [ _, _, _, _, _, _, _, _, _, _],
             [ _, _, _, _, _, _, _, _, _, _],
             [ _, _, _, _, _, _, _, _, _, _],
             [ _, _, _, _, _, _, _, _, _, _]],
    ai:bestMove(Board, 1, 2, X, Y),
    X == 4,
    Y == 6.

% test where ia must not choose the highest amount of case to swap (case before corner).
testNotCorner(bestMove) :-
    Board = [[ _, _, _, _, _, _, _, _, _, _],
             [ _, _, _, _, _, _, _, _, _, _],
             [ _, _, _, _, _, _, _, _, _, _],
             [ _, _, _, _, _, _,-1, _, _, _],
             [ _, _, _, _, _,-1, _, _, _, _],
             [ _, _, _, _, 1,-1, _, _, _, _],
             [ _, _, _, _, _, _, _, _, _, _],
             [ _, _, _, _, _, _, _, _, _, _],
             [ _, _, _, _, _, _, _, _, _, _],
             [ _, _, _, _, _, _, _, _, _, _]],
    ai:bestMove(Board, 1, 2, X, Y),
    X==5,
    Y==6.


% test where ia must not choose the highest amount of case to swap (corner is possible).
         testCorner(bestMove) :-
             Board = [[ _, _, _, _, _, _, _, _, _, _],
                      [ _, _, _, _, _, _, _, _, _, _],
                      [ _, _, _, _, _, _, _,-1, _, _],
                      [ _, _, _, _, _, _, 1, _, _, _],
                      [ _, _, _, _, _, _,-1, _, _, _],
                      [ _, _, _, _, _, _,-1, _, _, _],
                      [ _, _, _, _, _, _, _, _, _, _],
                      [ _, _, _, _, _, _, _, _, _, _],
                      [ _, _, _, _, _, _, _, _, _, _],
                      [ _, _, _, _, _, _, _, _, _, _]],
             ai:bestMove(Board, 1, 2, X, Y),
             X==1,
             Y==8.

% test where ia must not choose the highest amount of case to swap (along side).
testCorner(bestMove) :-
    Board = [[ _, _, _, _, _, _, _, _, _, _],
             [ _, _, _, _, _, _, _, _, _, _],
             [ _, _, _, _, _, _, _, _, _, _],
             [ _, _, _, _, _, _, _, _,-1, _],
             [ _, _, _, _, _, _, _, _,-1, _],
             [ _, _, _, _, _, _, _,-1, 1, _],
             [ _, _, _, _, _, _, _, _, _, _],
             [ _, _, _, _, _, _, _, _, _, _],
             [ _, _, _, _, _, _, _, _, _, _],
             [ _, _, _, _, _, _, _, _, _, _]],
    ai:bestMove(Board, 1, 2, X, Y),
    X==5,
    Y==8.

% test where ia must not choose the highest amount of case to swap (corner is possible).
testCorner(bestMove) :-
    Board = [[ _, _, _, _, _, _, _, _, _, _],
             [ _, _, _, _, _, _, _, _, _, _],
             [ _, _, _, _, _, _, _, _,-1, _],
             [ _, _, _, _, _, _, _, _,-1, _],
             [ _, _, _, _, _, _, _, _,-1, _],
             [ _, _,-1,-1,-1,-1,-1,-1, 1, _],
             [ _, _, _, _, _, _, _, _, _, _],
             [ _, _, _, _, _, _, _, _, _, _],
             [ _, _, _, _, _, _, _, _, _, _],
             [ _, _, _, _, _, _, _, _, _, _]],
    ai:bestMove(Board, 1, 2, X, Y),
    X==1,
    Y==8.

:- end_tests(basic_ai).














