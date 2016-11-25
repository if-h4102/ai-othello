:- module(display, [displayBoard/0]).

getVal(IndexRow, IndexColumn, Val)  :- board(Board),  nth0(IndexRow, Board, Column), nth0(IndexColumn, Column, Val).
board(Board) :- Board =  [[ _, _, _, _, _, _, _, _, _, _],
                          [ _, 1, 1, 1, 1, 1, 1, 1, 1, _],
                          [ _, _, _, _, _, _, _, _, _, _],
                          [ _,-1,-1,-1,-1,-1,-1,-1,-1, _],
                          [ _, _, _, _, _, _, _, _, _, _],
                          [ _, _, _, _, _, _, _, _, _, _],
                          [ _, _, _, _, _, _, _, _, _, _],
                          [ _, _,-1, _, _, _, _,-1, _, _],
                          [ _, _, _, _, _, _, _, _, _, _],
                          [ _, _, _, _, _, _, _, _, _, _]].


getVal(X, Y, Val)  :- board(Board),  nth0(X, Board, Column), nth0(Y, Column, Val).
displayCell(Val) :- var(Val), write('   |'), !.
displayCell(-1) :- write(' x |').
displayCell(1) :- write(' o |').

displayCell(X, NewX, Y) :- 
  getVal(X, Y, Val), 
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