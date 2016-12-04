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
