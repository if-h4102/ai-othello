:- use_module('../main', []).
:- use_module('../io/display', []).
:- use_module('../game/end-of-game', []).
:- use_module('../game/utils', []).
:- use_module('../ai/ai', []).

gameloop() :-
	writeln('What is the type of the first player which will use the symbol x ?'),
	writeln('Type -1 for a human player or a number between 0 and 3 to select an ai with the given level.'),
	readIntBounded(Player1Type, -2, 4),
	writeln('What is the type of the second player which will use the symbol o ?'),
	writeln('Type -1 for a human player or a number between 0 and 3 to select an ai with the given level.'),
	readIntBounded(Player2Type, -2, 4),
	main:board(Board),
	display:displayBoard(Board),
	determineFirstPlayer(FirstPlayer),
	play(Board,FirstPlayer, Player1Type, Player2Type).

%%%%% determineFirstPlayer(-FirstPlayer)
% Determine randomly the first player.
determineFirstPlayer(FirstPlayer) :- maybe(1, 2), FirstPlayer is -1, !.
determineFirstPlayer(FirstPlayer) :- FirstPlayer is 1.

%%%%% determinePlayerType(+Player1Type, +Player2Type, +Player, -PlayerType).
% Determine the type of a given player.
determinePlayerType(Player1Type, _, Player, PlayerType) :-
	Player == -1, PlayerType is Player1Type, !.
determinePlayerType(_, Player2Type, _, PlayerType) :-
	PlayerType is Player2Type.

%%%%% play(+Board, +Player, +Player1Type, +Player2Type)
% Play while the game isn't finished.
play(Board, Player, _, _) :-
	'end_of_game':gameOver(Board, Player),
	'end_of_game':winner(Board, Winner),
	display:displayWinner(Winner),
	!.
play(Board, Player, Player1Type, Player2Type) :-
	'end_of_game':playerCanPlay(Board, Player),
	determinePlayerType(Player1Type, Player2Type, Player, PlayerType),
	PlayerType == -1,
	humanPlay(Board, Player, X, Y),
	updateDisplayBoard(Board, Player, X, Y, Player1Type, Player2Type),
	!.
play(Board, Player, Player1Type, Player2Type) :-
	'end_of_game':playerCanPlay(Board, Player),
	determinePlayerType(Player1Type, Player2Type, Player, PlayerType),
	ai:bestMove(Board, Player, PlayerType, X, Y),
	updateDisplayBoard(Board, Player, X, Y, Player1Type, Player2Type),
	!.
play(Board, Player, Player1Type, Player2Type) :-
	NewPlayer is -Player,
	play(Board, NewPlayer, Player1Type, Player2Type).

%%%%% updateDisplayBoard(+Board, +Player, +X, +Y, +Player1Type, +Player2Type).
% Update and display the board taking into account the new play at (X, Y) coordinates.
updateDisplayBoard(Board, Player, X, Y, Player1Type, Player2Type) :-
	utils:updateBoard(Board, Player, X, Y, NewBoard),
	display:displayBoard(NewBoard),
	NewPlayer is -Player,
	play(NewBoard,NewPlayer, Player1Type, Player2Type).


%%%%% humanPlay(+Board, +Player, -X, -Y)
% Ask to a human player where he wants to play.
humanPlay(Board, Player, X, Y) :-
	printPlayerSymbol(Player),
	writeln('At which abscissa do you want to play ?'),
	readIntBounded(Xtmp, 0, 9),
	writeln('At which ordinate do you want to play ?'),
	readIntBounded(Ytmp, 0, 9),
	checkGivenCoordinates(Board, Player, X, Y, Xtmp, Ytmp).

%%%%% printPlayerSymbol(+Player)
% Print the symbol to the given player.
printPlayerSymbol(Player) :- Player == -1, writeln('You use the symbol : x'), !.
printPlayerSymbol(_) :- writeln('You use the symbol : o').

%%%%% checkGivenCoordinates(+Board, +Player, -X, -Y, +Xtmp, +Ytmp)
% Check if the coordinates given by the human player are valid.
checkGivenCoordinates(Board, Player, X, Y, Xtmp, Ytmp) :-
	'end_of_game':canBePlayed(Board, Xtmp, Ytmp, Player),
	X is Xtmp,
	Y is Ytmp, !.
checkGivenCoordinates(Board, Player, X, Y, _, _) :-
	writeln('The square you choosed is invalid, please choose an other one.'),
	humanPlay(Board, Player, X, Y).

%%%%% readInt(-X)
% read a user input until it is an integer.
% From the second input to the end, print ' Please enter an integer ' before each input.
readInt(X) :-
    read(X), integer(X), !.
readInt(X) :-
    repeat, write(' Please enter an integer '), read(X), integer(X), !.

%%%%% readIntBounded(-X, +Min, +Max)
% read a user input until it is an integer and is between Min and Max (exclusive).
redaIntBounded(_, Min, Max) :-
    Min > Max, !, fail.
readIntBounded(X, Min, Max) :-
    readInt(X), X < Max, X > Min, !.
readIntBounded(X, Min, Max) :-
    repeat, 
    write('The value must be between '), write(Min), write(' and '), write(Max), writeln(' (exclusive)'),
    readInt(X),
    X < Max,
    X > Min,
    !.


