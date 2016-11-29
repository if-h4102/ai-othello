:- use_module('../main', []).
:- use_module('../io/display', []).
:- use_module('../game/end-of-game', []).
:- use_module('../game/utils', []).
% Player type can be 0 (human) or 1 (ai).
gameloop() :-
	writeln('What is the type of the first player (0 for human, 1 for ai)'),
	read(Player1Type),
	writeln('What is the type of the second player (0 for human, 1 for ai)'),
	read(Player2Type),
	main:board(Board),
	display:displayBoard(Board),
	determineFirstPlayer(FirstPlayer),
	play(Board,FirstPlayer, Player1Type, Player2Type),
	write('Player 1 is a '), writeln(Player1Type),
	write('Player 2 is a '), writeln(Player2Type).

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
play(Board, Player, _, _) :- 'end-of-game':gameOver(Board, Player), !.
play(Board, Player, Player1Type, Player2Type) :-
	determinePlayerType(Player1Type, Player2Type, Player, PlayerType),
	PlayerType == 0,
	humanPlay(Board, Player, X, Y),
	utils:updateBoard(Board, Player, X, Y, NewBoard),
	display:displayBoard(NewBoard),
	NewPlayer is -Player,
	play(NewBoard,NewPlayer, Player1Type, Player2Type).
play(Board, Player, Player1Type, Player2Type) :-
	determinePlayerType(Player1Type, Player2Type, Player, PlayerType),
	PlayerType == 1,
	aiPlay(Board, Player).

%%%%% humanPlay(+Board, +Player, -X, -Y)
% Ask to a human player where he wants to play.
humanPlay(Board, Player, X, Y) :-
	printPlayerSymbol(Player),
	writeln('At which abscissa do you want to play ?'),
	read(Xtmp),
	writeln('At which ordinate do you want to play ?'),
	read(Ytmp),
	checkGivenCoordinates(Board, Player, X, Y, Xtmp, Ytmp).

%%%%% printPlayerSymbol(+Player)
% Print the symbol to the given player.
printPlayerSymbol(Player) :- Player == -1, writeln('You use the symbol : x'), !.
printPlayerSymbol(_) :- writeln('You use the symbol : o').

%%%%% checkGivenCoordinates(+Board, +Player, -X, -Y, +Xtmp, +Ytmp)
% Check if the coordinates given by the human player are valid.
checkGivenCoordinates(Board, Player, X, Y, Xtmp, Ytmp) :-
	'end-of-game':canBePlayed(Board, Xtmp, Ytmp, Player),
	X is Xtmp,
	Y is Ytmp, !.
checkGivenCoordinates(Board, Player, X, Y, _, _) :-
	writeln('The square you choosed is invalid, please choose an other one.'),
	humanPlay(Board, Player, X, Y).


% Ask to an ai where it wants to play.
aiPlay(_, _) :- writeln('Ai plays').
