:- module('random_ai', []).
:- use_module('../game/end-of-game', []).
:- use_module('utils_ai', []).

% The board is represented this way:
% [[-, ..., _], ..., [-, ..., _]]
% Matrix 10*10 ; Board[col][row]

% Returns the first coordinates (X, Y) of the next move if you call it this way:
% play(board, player, X, Y)
% This is the AI 0 : totally random
ai0(Board, Player, X, Y) :- 
    repeat, 
    X is (1+random(8)), 
    Y is (1+random(8)), 
    end_of_game:canBePlayed(Board, X, Y, Player).

% Computes the list of all possible moves if called this way:
% possibleMoves(board, player, PossibleMoves)
%possibleMoves(Board, Player, PossibleMoves) :- findall(Move, canBePlayed(Board, Player, Move), PossibleMoves).

% A bit better AI: AI 1
% Plays randomly one possible move amongst all possible moves
ai1(Board, Player, X, Y) :-
    utils_ai:possibleMoves(Board, Player, PossibleMoves),
    length(PossibleMoves, PossibleMovesLength),
    Index is random(PossibleMovesLength),
    nth0(Index, PossibleMoves, Move),
    utils_ai:getXYMove(Move, X, Y).
