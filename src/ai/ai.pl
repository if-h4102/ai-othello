:- module('ai', [bestMove/5, possibleMoves/3]).
:- use_module('random-ai', []).
:- use_module('basic-ai', []).
% :- use_module('min-max-ai', []).

%%%%% bestMove(+Board, +Player, +Ai, -X, -Y)
% Returns the best move (X, Y) according to the given Ai

% Totally random AI (Ai = 0)
bestMove(Board, Player, 0, X, Y) :- random_ai:ai0(Board, Player, X, Y).

% Still random but faster AI (Ai = 1)
bestMove(Board, Player, 1, X, Y) :- random_ai:ai1(Board, Player, X, Y).

% Basic AI (Ai = 2)
bestMove(Board, Player, 2, X, Y) :- basic_ai:bestMove(Board, X, Y, Player).

% Min-Max AI (Ai = 3)
% bestMove(Board, Player, 3, X, Y) :- min_max_ai:bestMove(Board, X, Y, Player).

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
