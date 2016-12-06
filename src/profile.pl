:- module('profile', []).
:- use_module('./main', []).
:- use_module('gameloop/gameloop', []).
:- use_module('gameloop/gameloop_auto', []).

playAllGames(_, _, 0) :-
    !.

playAllGames(Player1Type, Player2Type, Remaining) :-
    main:board(Board),
    % This is reversed at the first turn so start with -1
    FirstPlayer is -1,
    gameloop_auto:playAuto(Board, FirstPlayer, Player1Type, Player2Type),
    NextRemaining is Remaining - 1,
    playAllGames(Player1Type, Player2Type, NextRemaining),
    !.

runProfiling(Argv) :-
    length(Argv, 3),
    nth0(0, Argv, Player1TypeStr),
    nth0(1, Argv, Player2TypeStr),
    nth0(2, Argv, RunsStr),
    atom_number(Player1TypeStr, Player1Type),
    atom_number(Player2TypeStr, Player2Type),
    atom_number(RunsStr, Runs),
    playAllGames(Player1Type, Player2Type, Runs),
    !.

runProfiling(_) :-
    halt(1),
    !.

%%% Run a game from command line
% Positional arguments:
% p1: first player type
% p2: second player type
% nbRuns: number of matches to play
:-
    current_prolog_flag(argv, Argv),
    runProfiling(Argv),
    halt,
    !.
