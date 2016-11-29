:- module('display', [displayBoard/1]).
:- use_module('../game/utils', []).

displayCell(Val) :- var(Val), write('   |'), !.
displayCell(-1) :- write(' x |').
displayCell(1) :- write(' o |').

displayCell(Board, X, NewX, Y) :-
  utils:getVal(Board, X, Y, Val), 
  displayCell(Val), 
  NewX is X + 1.

displayRow(Board, Y, NewY):- 
  write('|'),
  displayRow(Board, 1, 9, Y),
  NewY is Y + 1,
  writeln(''),
 	writeln('---------------------------------').
  
displayRow(_, X, EndX, _) :-
  X is EndX.
  
displayRow(Board, X, EndX, Y) :-
  displayCell(Board, X, NewX, Y),
  displayRow(Board, NewX, EndX, Y).
  
displayBoard(Board) :- 
	writeln('---------------------------------'),
  displayBoard(Board, 1, 9).
 
displayBoard(_, Y, EndY) :- 
  Y is EndY, 
  !.
  
displayBoard(Board, Y, EndY) :- 
  displayRow(Board, Y, NewY),
  displayBoard(Board, NewY, EndY), 
  !.
