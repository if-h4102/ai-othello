:- module('test_score', [test_score/0]).
:- use_module(library(plunit)).
:- use_module('../src/game/end-of-game', []).

% To run this test case
test_score :-
	run_tests([score]).

% Define the test case
:- begin_tests(score).

test(countStartingScore) :-
	Board = [[ _, _, _, _, _, _, _, _, _, _],
             [ _, _, _, _, _, _, _, _, _, _],
             [ _, _, _, _, _, _, _, _, _, _],
             [ _, _, _, _, _, _, _, _, _, _],
             [ _, _, _, _, 1,-1, _, _, _, _],
             [ _, _, _, _,-1, 1, _, _, _, _],
             [ _, _, _, _, _, _, _, _, _, _],
             [ _, _, _, _, _, _, _, _, _, _],
             [ _, _, _, _, _, _, _, _, _, _],
             [ _, _, _, _, _, _, _, _, _, _]],
    end_of_game:getScoreBoard(Board, ActualScore),
    ActualScore == 0.

:- end_tests(score).
