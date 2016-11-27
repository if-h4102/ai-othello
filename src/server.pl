:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_json)).
:- use_module(library(http/json_convert)).

% This predicate allows us to launch the server on the port given as parameter
server(Port) :- http_server(http_dispatch, [port(Port)]).

%%%%%% Define API endpoints

% Send the initial board as a json array
% Requirements: none
% URL Parameters: none
% JSON header: none
% Result: json arrray 10 * 10
%         - 0 : empty case
%         - -1: white token
%         - 1 : black token
:- http_handler('/api/board/initial', api_board_initial, []).

% Send the updated board after the given move is played
% Requirements: the given move must be a valid one (see /api/play/validate)
% URL Parameters:
%   - player: the player playing the given move
%         - -1: white tokens player
%         - 1 : black tokens player
%   - movex: the x coordinate of the move (integer between 1 and 8)
%   - movey: the y coordinate of the move (integer between 1 and 8)
% JSON header: the current board as a json array
% Result: json arrray 10 * 10
%         - 0 : empty case
%         - -1: white token
%         - 1 : black token
:- http_handler('/api/board/update', api_board_update, []).

% Play one move as the given AI, and send back the updated board
% Requirements: the board must be a valid one (10*10 containing 0, -1 or 1 only)
% URL Parameters:
%   - ai: the AI according to which the move will be played
%         - 0: totally random AI
%         - 1: random first correct move AI
%         - 2: basic AI (compute the best instant move)
%         - 3: min-max AI
%   - player: the player which the AI will play for
%         - -1: white tokens player
%         - 1 : black tokens player
% JSON header: the current board as a json array
% Result: json arrray 10 * 10
%         - 0 : empty case
%         - -1: white token
%         - 1 : black token
:- http_handler('/api/play', api_play, []).

% Return true if the given move can be played, or false otherwise (in a JSON file)
% Requirements: the board must be a valid one (10*10 containing 0, -1 or 1 only)
% URL Parameters:
%   - player: the player which the AI will play for
%         - -1: white tokens player
%         - 1 : black tokens player
%   - movex: the x coordinate of the move (integer between 1 and 8)
%   - movey: the y coordinate of the move (integer between 1 and 8)
% JSON header: the current board as a json array
% Result: json object
% {
%   move: {
%     x: <movex>,
%     y: <movey>,
%   },
%   board: <the_given_board>,
%   playable: true | false
% }
:- http_handler('/api/play/validate', api_play_validate, []).

%%%%%%%%%%%%%%%%%%%%%%%%
% The basics !
% Add a basic route
:- http_handler(/, welcome, []).

% Add a basic request handler
welcome(_Request) :-
    format('Content-type: text/plain~n~n'),
    format('Hello World!~n').
    
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
    json_to_prolog(JsonIn, Object),
    format(user_output, "~p~n", [Object]),
    reply_json_dict(JsonIn).
:- http_handler('/json/echo', api_json_echo, []).
% Example with:
% curl -H "Content-Type: application/json" -X POST -d '{"Board":[{"id":0, "row":[{"id":0, "value":0}, {"id":1, "value":-1}]}]}' http://localhost:8000/json/echo

% And now, let's build an API endpoint which send a board as a json!
:- assert(testBoard([
    [0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0,-1, 1, 0, 0, 0, 0],
    [0, 0, 0, 0, 1,-1, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
])).
api_json_board(_) :-
    testBoard(Board),
    prolog_to_json(Board, JsonBoard),
    reply_json_dict(JsonBoard).
:- http_handler('/json/board', api_json_board, []).

:- server(8000).
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    