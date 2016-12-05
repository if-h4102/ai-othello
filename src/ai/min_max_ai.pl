:- module('min_max_ai', []).
:- use_module('../game/end-of-game', []).
:- use_module('../game/utils', []).
:- use_module('utils_ai', []).

% bestMove(+Board, -X, -Y, +CurrentPlayer
bestMove(Board, X, Y, CurrentPlayer) :- 
    utils_ai:possibleMoves(Board, CurrentPlayer, MoveList), 
    findBestMove(Board, MoveList, -99999, X, Y, CurrentPlayer), 
    !.

%findBestMove(+Board, +MoveList, +BestScore, -BestX, -BestY, +AiPlayer)
findBestMove(_, [], _, _, _, _) :- !.

findBestMove(Board, [Move|Tail], BestScore, BestX, BestY, AiPlayer) :-
    utils_ai:getXYMove(Move, X, Y), 
    write(X), write("."), write(Y), write("."), write(AiPlayer), write(" | "),
    utils:updateBoard(Board, AiPlayer, X, Y, NewBoard),
    HumanPlayer is -AiPlayer, 
    getScoreMinMax(NewBoard, HumanPlayer, 1, PlayerIndependantScore),
    Score is PlayerIndependantScore * HumanPlayer, 
    Score > BestScore, 
    findBestMove(Board, Tail, Score, NewBestX, NewBestY, AiPlayer), 
    ( % if a better score is found set return var with it else use the current one
        ( 
            var(NewBestX), 
            var(NewBestY), 
            BestX is X, 
            BestY is Y
        )
        ;
        ( 
            BestX is NewBestX,
            BestY is NewBestY
        )
    ), 
    !.

findBestMove(Board, [_|Tail], BestScore, BestX, BestY, Player) :- 
    findBestMove(Board, Tail, BestScore, BestX, BestY, Player), 
    !.
    
getScoreMinMax(Board, Player, 0, Score) :-
    utils_ai:getScoreBoard(Board, Player, ScoreDependantPlayer),
    Score is ScoreDependantPlayer * Player,
    writeln("Depth = 0").
    
getScoreMinMax(Board, CurrentPlayer, Depth, Score) :-
    utils_ai:possibleMoves(Board, CurrentPlayer, MoveList),
    findBestScore(Board, MoveList, CurrentPlayer, Depth, -999999, Score).
    



%findBestScore(+Board, +MoveList, +CurrentPlayer, +Depth, +CurrentBestScore, -FoundBestScore)
findBestScore(_, [], _, _, CurrentBestScore, FoundBestScore) :- 
    FoundBestScore is CurrentBestScore,
    format(user_output, '~n', []), 
    !.

findBestScore(Board, [Move|Tail], CurrentPlayer, Depth, CurrentBestScore, FoundBestScore) :-
    utils_ai:getXYMove(Move, X, Y), 
    write(X), write(" "), write(Y), write(" "), write(CurrentPlayer), write(" | "), 
    utils:updateBoard(Board, CurrentPlayer, X, Y, NewBoard), 
    NewDepth is Depth - 1,
    write("Depth : "), writeln(Depth),
    NextPlayer is -CurrentPlayer,
    getScoreMinMax(NewBoard, NextPlayer, NewDepth, PlayerIndependantScore),
    Score is PlayerIndependantScore * NextPlayer,
    write("Score : "), write(Score), write(" CurrentScore : "), writeln(CurrentBestScore), 
    Score > CurrentBestScore, 
    findBestScore(Board, Tail, CurrentPlayer, Depth, Score, FoundBestScore),
    (
        (
            var(FoundBestScore),
            CurrentBestScore is Score
        )
        ;
        (   
            CurrentBestScore is FoundBestScore
        )
    ),
    !.

findBestScore(Board, [_|Tail], CurrentPlayer, Depth, CurrentBestScore, FoundBestScore) :- 
    writeln("Skip method"),
    findBestScore(Board, Tail, CurrentPlayer, Depth, CurrentBestScore, FoundBestScore), 
    !.



















