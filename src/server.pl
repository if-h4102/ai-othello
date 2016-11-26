:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).

% Add a basic route
:- http_handler(/, welcome, []).

% Add a basic request handler
welcome(_Request) :-
    format('Content-type: text/plain~n~n'),
    format('Hello World!~n').

% This predicate allows us to launch the server on the port given as parameter
server(Port) :- http_server(http_dispatch, [port(Port)]).
