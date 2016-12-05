:- module('basic_ai', []).
:- use_module('../game/end-of-game', []).
:- use_module('utils_ai', []).


%testBoard(Board) :- 
%    Board =[
%    [_, _, _, _, _, _, _, _, _, _], 
%    [_, _, _, _, _, _, _, _, _, _], 
%    [_, _, _, _, _, _, _, _, _, _], 
%    [_, _, _, _, _, _, _, _, _, _], 
%    [_, _, _, _, -1, 1, _, _, _, _], 
%    [_, _, _, _, 1, -1, _, _, _, _], 
%    [_, _, _, _, _, _, _, _, _, _], 
%    [_, _, _, _, _, _, _, _, _, _], 
%    [_, _, _, _, _, _, _, _, _, _], 
%    [_, _, _, _, _, _, _, _, _, _]
%    ].




bestMove(Board, X, Y, Player) :- utils_ai:possibleMoves(Board, Player, PossibleMoves), foundBestMove(Board, PossibleMoves, -99999, X, Y, Player), !.

%foundBestMove(+Board, +MoveList, +BestScore, -BestX, -BestY, +Player)
foundBestMove(_, [], _, _, _, _) :- format(user_output, '', []), !.

foundBestMove(Board, [Move|Tail], BestScore, BestX, BestY, Player) :-
    utils_ai:getXYMove(Move, X, Y), 
    utils:updateBoard(Board, Player, X, Y, NewBoard), 
    utils_ai:getScoreBoard(NewBoard, Player, Score), 
    Score > BestScore, 
    foundBestMove(Board, Tail, Score, NewBestX, NewBestY, Player), 
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

foundBestMove(Board, [_|Tail], BestScore, BestX, BestY, Player) :- 
    foundBestMove(Board, Tail, BestScore, BestX, BestY, Player), 
    !.
    


    
    
    
    
    
    
    
    
