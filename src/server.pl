:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_json)).

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
    square(Number, Res),
    reply_json(json([number=Number, square=Res])).
:- http_handler('/square', api_square, []).

%%%%%%%%%%%%%%%%%%%%%%%%
% And now, let's try to translate JSON to Prolog and vice versa
% First build a handler able to echo a json file
api_json_echo(Request) :-
    http_read_json_dict(Request, JsonIn),
    reply_json_dict(JsonIn).
:- http_handler('/json/echo', api_json_echo, []).

:- server(8000).
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    