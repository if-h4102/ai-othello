:- module('ai', []).
:- use_module('random-ai', []).
:- use_module('basic-ai', []).

%%%%% bestMove(Board, Player, Ai, X, Y)
% Returns the best move (X, Y) according to the given Ai

% Random AI (Ai = 0)
bestMove(Board, Player, 0, X, Y) :- true.

% Basic AI (Ai = 1)
bestMove(Board, Player, 1, X, Y) :- basic_ai:bestMove(Board, X, Y, Player).