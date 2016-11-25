:- use_module(library(plunit)).
:- use_module(score, []).

% Run all tests
:- run_tests(score), halt(0).	% Exit with 0 when all tests passed
:- halt(1).						% Or exit with 1 when at least one test failed
