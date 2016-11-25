:- use_module(io/display, []).

% Define the predicate which will unify with the current state of the board game.
:- dynamic board/1.		% "dynamic" means that predicate's definition may change during run time

% Initialize the board
% "assert" put the board in the knowledges base.
:- assert(board([
    [_, _, _, _, _, _, _, _, _, _],
    [_, _, _, _, _, _, _, _, _, _],
    [_, _, _, _, _, _, _, _, _, _],
    [_, _, _, _, _, _, _, _, _, _],
    [_, _, _, _,-1, 1, _, _, _, _],
    [_, _, _, _, 1,-1, _, _, _, _],
    [_, _, _, _, _, _, _, _, _, _],
    [_, _, _, _, _, _, _, _, _, _],
    [_, _, _, _, _, _, _, _, _, _],
    [_, _, _, _, _, _, _, _, _, _]
])).

run :- writeln('Running...'),
	display:displayBoard,
	writeln('End!').

:- run.
