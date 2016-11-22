% Import the module in the file at the path 'a.pl' -> will import 'a1' since a.pl defines the module a1.
% The empty brackets prevent the module from extending the global namespace automaticall.
% If you want to augment the current namespace, specify the predicates (name/arity) in the brackets.
% Example: 
% :- use_module(a, [gt/2]).
:- use_module(a, []).
:- use_module(b, []).

gt3(X, Y) :- a1:gt(X, Y).
