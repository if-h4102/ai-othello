:- module('gameloop_auto', []).
:- use_module('../main', []).
:- use_module('../io/display', []).
:- use_module('../game/end-of-game', []).
:- use_module('../game/utils', []).
:- use_module('../ai/ai', []).
:- use_module('../gameloop/gameloop',[]).


%%%%%%%%% Automatically create many games
gameloop_auto(NbGames) :-
	NbGames==0,
	!.

gameloop_auto(NbGames) :-
    write(NbGames),
    write(' '),
    main:board(Board),
	gameloop:determineFirstPlayer(FirstPlayer),
	playAuto(Board, FirstPlayer, 1 , 1),
	NbGamesn is NbGames-1,
	gameloop_auto(NbGamesn),
	!.

% Update but don't display board
updateBoard(Board, Player, X, Y, Player1Type, Player2Type) :-
    utils:updateBoard(Board, Player, X, Y, NewBoard),
    NewPlayer is -Player,
    playAuto(NewBoard,NewPlayer, Player1Type, Player2Type).

% Play while the game isn't finished and don't display board.
playAuto(Board, Player, _, _) :-
	'end_of_game':gameOver(Board, Player),
	'end_of_game':winner(Board, Winner),
	Winner == 1,
	writeln('1'),
	!.
playAuto(Board, Player, _, _) :-
	'end_of_game':gameOver(Board, Player),
	'end_of_game':winner(Board, Winner),
	Winner == -1,
	% Player -1 won, output as "2"
	writeln('2'),
	!.
playAuto(Board, Player, _, _) :-
	'end_of_game':gameOver(Board, Player),
	% Draw, output as "0"
	writeln('0'),
	!.
playAuto(Board, Player, Player1Type, Player2Type) :-
	'end_of_game':playerCanPlay(Board, Player),
	gameloop:determinePlayerType(Player1Type, Player2Type, Player, PlayerType),
	ai:bestMove(Board, Player, PlayerType, X, Y),
	updateBoard(Board, Player, X, Y, Player1Type, Player2Type),
	!.
playAuto(Board, Player, Player1Type, Player2Type) :-
	NewPlayer is -Player,
	playAuto(Board, NewPlayer, Player1Type, Player2Type).
