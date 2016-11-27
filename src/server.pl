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
% Requirements: - the given move must be a valid one (see /api/play/validate)
%               - the given board must be a valid one (10*10 containing 0, -1 or 1 only)
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
% Requirements: the given board must be a valid one (10*10 containing 0, -1 or 1 only)
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
% Requirements: the given board must be a valid one (10*10 containing 0, -1 or 1 only)
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


%%%%%% Define API calls' handlers

% /api/board/initial
initialBoard([
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
]).
api_board_initial(_) :-
    initialBoard(Board),
    prolog_to_json(Board, JsonBoard),
    reply_json_dict(JsonBoard).
    
% /api/board/update
% Requirements: - the given move must be a valid one (see /api/play/validate)
%               - the given board must be a valid one (10*10 containing 0, -1 or 1 only)
api_board_update(Request) :-
    http_parameters(Request, [
      player(Player, [integer, oneof([-1, 1])]),
      movex(X, [integer, between(1, 8)]),
      movey(Y, [integer, between(1, 8)])
    ]),
    http_read_json_dict(Request, JsonBoard),
    json_to_prolog(JsonBoard, Board),
    % TODO: import method that generates the next board
    NextBoard = 'This API endpoint is not yet implemented',
    reply_json_dict(json([error=NextBoard])).
    
% /api/play
% Requirements: the given board must be a valid one (10*10 containing 0, -1 or 1 only)
api_play(Request) :-
    http_parameters(Request, [
      player(Player, [integer, oneof([-1, 1])]),
      ai(Ai, [integer, between(0, 3)])
    ]),
    http_read_json_dict(Request, JsonBoard),
    json_to_prolog(JsonBoard, Board),
    % TODO: import method that plays and generates the next board
    NextBoard = "This API endpoint is not yet implemented",
    reply_json_dict(json([error=NextBoard])).
    
% /api/play/validate
% Requirements: the given board must be a valid one (10*10 containing 0, -1 or 1 only)
api_play_validate(Request) :-
    http_parameters(Request, [
      player(Player, [integer, oneof([-1, 1])]),
      movex(X, [integer, between(1, 8)]),
      movey(Y, [integer, between(1, 8)])
    ]),
    http_read_json_dict(Request, JsonBoard),
    json_to_prolog(JsonBoard, Board),
    % TODO: import method which validates or not the move
    Result = "This API endpoint is not yet implemented",
    Playable = false,
    reply_json_dict(json([error=Result, move=json([x=X, y=Y]), board=Board, playable=Playable])).
    



:- server(8000).
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    