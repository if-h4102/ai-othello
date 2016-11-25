:- module(utils).


getVal(X, Y, Val)  :- 
  board(Board),  
  nth0(Y, Board, Column), 
  nth0(X, Column, Val).

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