:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).

% Add a basic route
:- http_handler(/, welcome, []).

% Add a basic request handler
welcome(_Request) :-
    format('Content-type: text/plain~n~n'),
    format('Hello World!~n').

% This predicate allows us to launch the server on the port given as parameter
server(Port) :- http_server(http_dispatch, [port(Port)]).


%%%%%%%%%%%%%%%%%%%%%%%%
% Let's try to build an API endpoint,
% which send back the square number of the given X
square(X, Res) :- Res is X * X .
api_square(Request) :-
    http_parameters(Request, [number(Number, [number])]),
    format('Content-type: text/plain~n~n'),
    square(Number, Res),
    format(Res).
:- http_handler('/square', api_square, []).