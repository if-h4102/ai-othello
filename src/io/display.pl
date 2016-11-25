:- module(display, [displayBoard/0]).
:- use_module(game/utils, []).

displayCell(Val) :- var(Val), write('   |'), !.
displayCell(-1) :- write(' x |').
displayCell(1) :- write(' o |').

displayCell(X, NewX, Y) :-
  utils:getVal(X, Y, Val), 
  displayCell(Val), 
  NewX is X + 1.

displayRow(Y, NewY):- 
  write('|'),
  displayRow(1, 9, Y),
  NewY is Y + 1,
  writeln(''),
 	writeln('---------------------------------').
  
displayRow(X, EndX, Y) :-
  X is EndX.
  
displayRow(X, EndX, Y) :-
  displayCell(X, NewX, Y),
  displayRow(NewX, EndX, Y).
  
displayBoard :- 
	writeln('---------------------------------'),
  displayBoard(1, 9).
 
displayBoard(Y, EndY) :- 
  Y is EndY, 
  !.
  
displayBoard(Y, EndY) :- 
  displayRow(Y, NewY),
  displayBoard(NewY, EndY), 
  !.