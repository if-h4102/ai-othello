:- module(utils, [getVal/4, isCaseEmpty/3, isOnBoard/2, nextCase/4]).

getVal(Board, X, Y, Val)  :- 
  nth0(X, Board, Column), 
  nth0(Y, Column, Val).
  
isCaseEmpty(Board, X, Y) :-
  getVal(Board, X, Y, Val),
  var(Val).

isOnBoard(X, Y) :- 
  X > 0, X < 9, Y > 0, Y < 9.
  
nextCase(X, Y, _, _) :-
  isOnBoard(X, Y),
  X == 8,
  Y == 8,
  !,
  fail.

nextCase(X, Y, NewX, NewY) :-
  isOnBoard(X, Y),
  X == 8,
  NewX is 1,
  NewY is Y + 1.
  
nextCase(X, Y, NewX, NewY) :-
  isOnBoard(X, Y),
  NewX is X + 1,
  NewY is Y.

nextBoard(OldBoard, NewBoard) :-
  nextBoard(OldBoard, NewBoard, 1, 1).
  
nextBoard(OldBoard, NewBoard, X, Y) :-
  getVal(NewBoard, X, Y, NewVal),
  var(NewVal),
  getVal(OldBoard, X, Y, OldVar),
  getVal(NewBoard, X, Y, OldVar),
  nextCase(X, Y, NewX, NewY),
  nextBoard(OldBoard, NewBoard, NewX, NewY).