:- module('test_basic_ai', []).
:- use_module(library(plunit)).
:- use_module('../src/ai/basic-ai', []).

% To run this test case
test_basic_ai :-
	run_tests([basic_ai]).

% Define the test case
:- begin_tests(basic_ai).  

:- end_tests(basic_ai).
