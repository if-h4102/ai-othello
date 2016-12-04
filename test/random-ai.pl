:- module('test_random_ai', []).
:- use_module(library(plunit)).
:- use_module('../src/ai/random-ai', []).

% To run this test case
test_random_ai :-
	run_tests([random_ai]).

% Define the test case
:- begin_tests(random_ai).

:- end_tests(random_ai).
