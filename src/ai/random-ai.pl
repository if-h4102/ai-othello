% Uncomment to define a module

% :-  module(ModuleName, [ai1/4, canBePlayed/3])
% :-  use_module(printActors). 

% TODO: import this from another file
% This function returns coordinates (X, Y) on the given Board where Player can play
canBePlayed(Board, X, Y, Player) :- true;
% This function returns one possible move
canBePlayed(Board, Player, Move) :- nth0(0, Move, X), nth0(1, Move, Y) canBePlayed(Board, X, Y, Player).

% The board is represented this way:
% [[-, ..., _], ..., [-, ..., _]]
% Matrix 10*10 ; Board[col][row]

% This function returns the first coordinates (X, Y) of the next move if you call it this way:
% play(board, player, X, Y)
% This is the AI 0 : f****** random
ai0(Board, Player, X, Y) :- repeat, X is (1+random(8)), Y is (1+random(8)), canBePlayed(Board, X, Y, Player).

% Compute the list of all possible moves if called this way:
% possibleMoves(board, player, PossibleMoves)
possibleMoves(Board, Player, PossibleMoves) :- findAll(Move, canBePlayed(Board, Player, Move), PossibleMoves).

% A bit better AI: AI 1
% Plays randomly one possible move amongst all possible moves
ai1(Board, Player, X, Y) :-
	possibleMoves(Board, Player, PossibleMoves),
	length(PossibleMoves, PossibleMovesLength),
	Index is random(PossibleMovesLength),
	nth0(Index, PossibleMoves, Move),
	nth0(0, Move, X),
	nth0(1, Move, Y).

% Compute the next board
nextBoard(Board, X, Y, Player, NextBoard) :- Board = NextBoard, nth0(X, NextBoard, Col), nth0(Y, Col, Player).
