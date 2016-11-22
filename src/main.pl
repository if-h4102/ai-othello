:- use_module(io/display, []).

run :- writeln('Running...'),
	display:displayBoard,
	writeln('End!').

:- run.
