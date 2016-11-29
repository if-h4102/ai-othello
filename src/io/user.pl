:- module('user', [readIntInput/2]).

readIntInput(X) :-
    repeat, write(' Please enter an integer '), read(X), integer(X), !.

readIntInput(X, Y) :- 
  write('X ?'),
  readIntInput(X),
  write('Y ?'),
  readIntInput(Y).