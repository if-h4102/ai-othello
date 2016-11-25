:- module(utils, [getVal/3, invertVal/2, isCaseEmpty/2, isOnBoard/2]).

getVal(X, Y, Val)  :- 
  board(Board),  
  nth0(X, Board, Column), 
  nth0(Y, Column, Val).

% if case (X, Y) is empty, return false and stop
invertVal(X, Y) :-
  isCaseEmpty(X, Y),
  !, 
  fail.
  
% if case (X, Y) is not empty, invert the color 
invertVal(X, Y) :-
  getVal(X, Y, Val),
  NewVal is -Val,
  getVal(X, Y, NewVal).
  
isCaseEmpty(X, Y) :-
  getVal(X, Y, Val),
  var(Val).

isOnBoard(X,Y) :- X > 0, X < 9, Y > 0, Y < 9.