% Export the file 'a.pl' as the module named 'a1' ('a1' will be the name of the namespace).
% Usage (after import):
% a1:gt(5, 4).
:-  module(a1, [gt/2]).

gt(X, Y) :- X > Y.
