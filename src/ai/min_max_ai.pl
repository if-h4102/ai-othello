:- module('min_max_ai', []).
:- use_module('../game/end-of-game', []).
:- use_module('../game/utils', []).
:- use_module('utils_ai', []).


testBoard(Board) :- 
    Board =[
    [_, _, _, _, _, _, _, _, _, _], 
    [_, _, _, _, _, _, _, _, _, _], 
    [_, _, _, _, _, _, _, _, _, _], 
    [_, _, _, _, _, _, _, _, _, _], 
    [_, _, _, _, -1, 1, _, _, _, _], 
    [_, _, _, _, 1, -1, _, _, _, _], 
    [_, _, _, _, _, _, _, _, _, _], 
    [_, _, _, _, _, _, _, _, _, _], 
    [_, _, _, _, _, _, _, _, _, _], 
    [_, _, _, _, _, _, _, _, _, _]
    ].



% bestMove(+Board, -X, -Y, +CurrentPlayer
bestMove(Board, X, Y, CurrentPlayer) :- 
    utils_ai:possibleMoves(Board, CurrentPlayer, MoveList), 
    findBestMove(Board, MoveList, -9999999, X, Y, CurrentPlayer),
    !.

%findBestMove(+Board, +MoveList, +BestScore, -BestX, -BestY, +AiPlayer)
findBestMove(_, [], BestScore, _, _, AiPlayer) :- 
    !.

findBestMove(Board, [Move|Tail], BestScore, BestX, BestY, AiPlayer) :-
    utils_ai:getXYMove(Move, X, Y), 
    utils:updateBoard(Board, AiPlayer, X, Y, NewBoard),
    HumanPlayer is -AiPlayer, 
    getScoreMinMax(NewBoard, HumanPlayer, 2, PlayerIndependantScore),
    Score is PlayerIndependantScore * AiPlayer, 
    Score > BestScore, 
    findBestMove(Board, Tail, Score, BestX, BestY, AiPlayer), 
%    ( % if a better score is found set return var with it else use the current one
%        ( 
%            var(NewBestX), 
%            var(NewBestY), 
%            BestX is X, 
%            BestY is Y,
%            write("update best move : "), write(X), write(";"), writeln(Y),
%        )
%        ;
%        ( 
%            BestX is NewBestX,
%            BestY is NewBestY
%        )
%    ),
    updateBestMove(BestX,BestY,X,Y),
    !.
   
findBestMove(Board, [_|Tail], BestScore, BestX, BestY, Player) :- 
    findBestMove(Board, Tail, BestScore, BestX, BestY, Player), 
    !.

    
updateBestMove(OldX,OldY,NewX,NewY) :-
    var(OldX),
    var(OldY),
    OldX is NewX,
    OldY is NewY,
    !. 
    
updateBestMove(_,_,_,_).
    
    
    
getScoreMinMax(Board, Player, 0, PlayerIndependantScore) :-
    utils_ai:getScoreBoard(Board, Player, ScoreDependantPlayer),
    PlayerIndependantScore is ScoreDependantPlayer * Player,
    !.
    
getScoreMinMax(Board, CurrentPlayer, Depth, PlayerIndependantScore) :-
    utils_ai:possibleMoves(Board, CurrentPlayer, MoveList),
    findBestScore(Board, MoveList, CurrentPlayer, Depth, -999999, FoundScore),
    minMaxSetIndependantScore(CurrentPlayer, FoundScore, PlayerIndependantScore),
    !.

minMaxSetIndependantScore(Player, DependantScore, IndependantScore) :-
    IndependantScore is DependantScore * Player.


%findBestScore(+Board, +MoveList, +CurrentPlayer, +Depth, +CurrentBestScore, -FoundBestScore)
findBestScore(_, [], CurrentPlayer, _, -999999, FoundBestScore) :- 
    FoundBestScore is CurrentPlayer * 999999 ,
    !.
    
findBestScore(_, [], _, _, CurrentBestScore, FoundBestScore) :- 
    FoundBestScore is CurrentBestScore,
    !.

findBestScore(Board, [Move|Tail], CurrentPlayer, Depth, CurrentBestScore, FoundBestScore) :-
    utils_ai:getXYMove(Move, X, Y), 
    utils:updateBoard(Board, CurrentPlayer, X, Y, NewBoard), 
    NewDepth is Depth - 1,
    NextPlayer is -CurrentPlayer,
    getScoreMinMax(NewBoard, NextPlayer, NewDepth, PlayerIndependantScore),
    Score is PlayerIndependantScore * CurrentPlayer,
    Score > CurrentBestScore, 
    findBestScore(Board, Tail, CurrentPlayer, Depth, Score, FoundBestScore),
    !.

findBestScore(Board, [_|Tail], CurrentPlayer, Depth, CurrentBestScore, FoundBestScore) :- 
    findBestScore(Board, Tail, CurrentPlayer, Depth, CurrentBestScore, FoundBestScore), 
    !.



















