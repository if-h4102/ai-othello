:- module('ai', []).
:- use_module('random-ai', []).
:- use_module('basic-ai', []).
:- use_module('min_max_ai', []).

:-use_module('../io/display.pl', []).

%%%%% bestMove(+Board, +Player, +Ai, -X, -Y)
% Returns the best move (X, Y) according to the given Ai

% Totally random AI (Ai = 0)
bestMove(Board, Player, 0, X, Y) :- random_ai:ai0(Board, Player, X, Y).

% Still random but faster AI (Ai = 1)
bestMove(Board, Player, 1, X, Y) :- random_ai:ai1(Board, Player, X, Y).

% Basic AI (Ai = 2)
bestMove(Board, Player, 2, X, Y) :- basic_ai:bestMove(Board, X, Y, Player).

% Min-Max AI (Ai = 3)
bestMove(Board, Player, 3, X, Y) :- min_max_ai:bestMove(Board, X, Y, Player).

