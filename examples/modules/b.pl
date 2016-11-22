% Export the file 'a.pl' as the module named 'b' (recommended: use the name of the file)
:-  module(b, [gt/2]).

gt(X, Y) :- X < Y.
