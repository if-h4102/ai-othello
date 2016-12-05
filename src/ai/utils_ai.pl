:- module('utils_ai', []).
:- use_module('../game/end-of-game', []).

% This function returns one possible move,
% wrapped in an array of size 2.
canBePlayed(Board, Player, Move) :- 
    length(Move, 2), % Create an empty list of size 2.
    nth0(0, Move, X), 
    nth0(1, Move, Y), 
    end_of_game:canBePlayed(Board, X, Y, Player).

% possibleMoves(+Board, +Player, -PossibleMoves)
% Compute the list of all possible moves, and return a list on the format : [[X1, Y1], [X2, Y2], ..., [Xn, Yn]].
possibleMoves(Board, Player, PossibleMoves) :- findall(Move, canBePlayed(Board, Player, Move), PossibleMoves).

% getXYMove(+Move, -X, -Y)
% return the X value and Y value of a Move, representing at the format [X, Y].
getXYMove(Move, X, Y) :-
    nth0(0, Move, X),
    nth0(1, Move, Y).

scoreCoefBoard(ScoreBoard) :- ScoreBoard = [[0, 0   , 0  , 0  , 0  , 0  , 0  , 0  , 0   , 0], 
                                            [0, 1000, -30, 100, 100, 100, 100, -30, 1000, 0], 
                                            [0, -30 , -50, -5 , -5 , -5 , -5 , -50, -30 , 0], 
                                            [0, 100 , -5 , 1  , 1  , 1  , 1  , -5 , 100 , 0], 
                                            [0, 100 , -5 , 1  , 1  , 1  , 1  , -5 , 100 , 0], 
                                            [0, 100 , -5 , 1  , 1  , 1  , 1  , -5 , 100 , 0], 
                                            [0, 100 , -5 , 1  , 1  , 1  , 1  , -5 , 100 , 0], 
                                            [0, -30 , -50, -5 , -5 , -5 , -5 , -50, -30 , 0], 
                                            [0, 1000, -30, 100, 100, 100, 100, -30, 1000, 0], 
                                            [0, 0   , 0  , 0  , 0  , 0  , 0  , 0  , 0   , 0]].

getScoreBoard(Board, Player, Score) :-
    getScoreBoard(Board, Player, 1, 1, IndependantPlayerScore),
    Score is IndependantPlayerScore * Player. % By multipling by player, the ai must always maximise the score.

getScoreBoard(_, _, 1, 9, Score) :- 
    Score is 0,
    !.

getScoreBoard(Board, Player, X, Y, Score) :-
    getScoreCell(Board, X, Y, ScoreCell),
    utils:nextCase(X, Y, NewX, NewY),
    getScoreBoard(Board, Player, NewX, NewY, LeftOverScore),
    Score is ScoreCell + LeftOverScore,
    !.

% getScoreCell(+Board, +X, +Y, -Score)
% returns the score associated with this cell, which is 0 is the cell is empty
% else Player*CoefCell (with Player which is 1 or -1, and CoefCell the coef of the cell, provided by scoreCoefBoard). 
getScoreCell(Board, X, Y, Score) :-
    utils:getVal(Board, X, Y, Val),
    var(Val),
    Score is 0,
    !.

getScoreCell(Board, X, Y, Score) :-
    scoreCoefBoard(Coef),
    utils:getVal(Board, X, Y, Val),
    utils:getVal(Coef, X, Y, CoefCell),
    Score is Val * CoefCell.
