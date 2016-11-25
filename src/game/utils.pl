:- module(utils, [getVal/3, isCaseEmpty/2, isOnBoard/2, nextCase/4]).

getVal(X, Y, Val)  :- 
  board(Board),  
  nth0(X, Board, Column), 
  nth0(Y, Column, Val).
  
isCaseEmpty(X, Y) :-
  getVal(X, Y, Val),
  var(Val).

isOnBoard(X,Y) :- 
  X > 0, X < 9, Y > 0, Y < 9.
  
nextCase(X, Y, NewX, NewY) :-
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
  
% Private Method
getVal(X, Y, Board, Val)  :-  
  nth0(X, Board, Column), 
  nth0(Y, Column, Val).
  
nextBoard(OldBoard, NewBoard, X, Y) :-
  getVal(X, Y, NewBoard, NewVal),
  var(NewVal),
  getVal(X, Y, OldBoard, OldVar),
  getVal(X, Y, NewBoard, OldVar),
  nextCase(X, Y, NewX, NewY),
  nextBoard(OldBoard, NewBoard, NewX, NewY).