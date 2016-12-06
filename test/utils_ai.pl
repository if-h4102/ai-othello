:- module('test_utils_ai', [test_utils_ai/0]).
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

test(get_x_y_move) :-
	utils_ai:getXYMove([3,5], X, Y),
	X == 3,
	Y == 5.

test(get_score_board) :-
	Board = [[ _, _, _, _, _, _, _, _, _, _],
		 [ _, _, _, _, _, _, _, _, _, _],
		 [ _, _, _, _, _, _, _, _, _, _],
		 [ _, _, _, _, _, _, _, _, _, _],
		 [ _, 1,-1,-1, 1,-1, _, _, _, _],
		 [ _, _, _, _, _, 1, _, _, _, _],
		 [ _, _, _, _, _, _, _, _, _, _],
		 [ _, _, _, _, _, _, _, _, _, _],
		 [ _, _, _, _, _, _, _, _, _, _],
		 [ _, _, _, _, _, _, _, _, _, _]],
	utils_ai:getScoreBoard(Board,1,Score),
	Score == 105.

test(scoreCase) :-
    Board = [[ _, _, _, _, _, _, _, _, _, _],
             [ _,-1, 1, -1, _, _, _, _, _, _],
             [ _,-1, 1, 1, _, _, _, _, _, _],
             [ _, 1,-1, _, _, _, _, _, _, _],
             [ _, _, _, _,-1, 1, _, _, _, _],
             [ _, _, _, _, _, _, _, _, _, _],
             [ _, _, _, _, _, _, _, _, _, _],
             [ _, _,-1, _, _, _, _, _, _, _],
             [ _, 1, _, _, _, _, _, _, _, _],
             [ _, _, _, _, _, _, _, _, _, _]],
    utils_ai:getScoreCell(Board, 1, 1, Score1),  Score1  == -1000,
    utils_ai:getScoreCell(Board, 2, 1, Score2),  Score2  == 30,
    utils_ai:getScoreCell(Board, 3, 1, Score3),  Score3  == 100,
    utils_ai:getScoreCell(Board, 8, 1, Score4),  Score4  == 1000,
    utils_ai:getScoreCell(Board, 1, 2, Score5),  Score5  == -30,
    utils_ai:getScoreCell(Board, 2, 2, Score6),  Score6  == -50,
    utils_ai:getScoreCell(Board, 3, 2, Score7),  Score7  == 5,
    utils_ai:getScoreCell(Board, 7, 2, Score8),  Score8  == 50,
    utils_ai:getScoreCell(Board, 1, 3, Score9),  Score9  == -100,
    utils_ai:getScoreCell(Board, 2, 3, Score10), Score10 == -5,
    utils_ai:getScoreCell(Board, 4, 4, Score11), Score11 == -1,
    utils_ai:getScoreCell(Board, 4, 5, Score12), Score12 == 1.

:- end_tests(utils_ai).
