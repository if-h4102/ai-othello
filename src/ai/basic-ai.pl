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


scoreCoefBoard(ScoreBoard) :- ScoreBoard = [[0, 0   , 0  , 0  , 0  , 0  , 0  , 0  , 0   , 0], 
                                            [0, 1000, -30, 100, 100, 100, 100, -30, 1000, 0], 
                                            [0, -30 , -50, -5 , -5 , -5 , -5 , -50, -30 , 0], 
                                            [0, 100 , -5 , 1  , 1  , 1  , 1  , -5 , 100 , 0], 
                                            [0, 100 , -5 , 1  , 1  , 1  , 1  , -5 , 100 , 0], 
                                            [0, 100 , -5 , 1  , 1  , 1  , 1  , -5 , 100 , 0], 
                                            [0, 100 , -5 , 1  , 1  , 1  , 1  , -5 , 100 , 0], 
                                            [0, -30 , -50, -5 , -5 , -5 , -5 , -50, -30 , 0], 
                                            [0, 1000, -30, 100, 100, 100, 100, -30, 1000, 0], 
                                            [0, 0   , 0  , 0  , 0  , 0  , 0  , 0  , 0   , 0]].

getScoreBoard(Board, Score, Player) :- 
    getScoreBoard(Board, PlayerIndependantScore, 1, 1), 
    Score is PlayerIndependantScore * Player.

getScoreBoard(_, Score, 8, 9) :- 
    Score is 0, 
    !.

%% if we try to go too far on Y increment X
getScoreBoard(Board, Score, LastX, 9) :- 
    X is LastX+1, 
    getScoreBoard(Board, Score, X, 1), 
    !.

%% if the case is unset, score don't change
getScoreBoard(Board, Score, LastX, LastY) :- 
    utils:getVal(Board, LastX, LastY, Case), 
    var(Case),
    Y is LastY+1, 
    getScoreBoard(Board, Score, LastX, Y),
    !.

%% else add the case value to the currant score
getScoreBoard(Board, Score, LastX, LastY) :- 
    utils:getVal(Board, LastX, LastY, Case), 
    Y is LastY+1, 
    getScoreBoard(Board, OldScore, LastX, Y),
    scoreCoefBoard(Coef),
    utils:getVal(Coef, LastX, LastY, CellCoef),
    Score is OldScore + Case * CellCoef, !.



getScoreBoard2(Board, Score, Player) :-
    getScoreBoard2(Board, Player, 1, 1, IndependantPlayerScore),
    Score is IndependantPlayerScore * Player. % By multipling by player, the ai must always maximise the score.

getScoreBoard2(_, _, 1, 9, Score) :- 
    Score is 0,
    !.

getScoreBoard2(Board, Player, X, Y, Score) :-
    getScoreCell(Board, X, Y, ScoreCell),
    utils:nextCase(X, Y, NewX, NewY),
    getScoreBoard2(Board, Player, NewX, NewY, LeftOverScore),
    Score is ScoreCell + LeftOverScore,
    !.

% getScoreCell(+Board, +X, +Y, -Score)
% returns the score associated with this cell, which is 0 is the cell is empty
% else Player*CoefCell (with Player which is 1 or -1, and CoefCell the coef of the cell, provided by scoreCoefBoard). 
getScoreCell(Board, X, Y, Score) :-
    utils:getVal(Board, X, Y, Val),
    var(Val),
    Score is 0,
    !.

getScoreCell(Board, X, Y, Score) :-
    scoreCoefBoard(Coef),
    utils:getVal(Board, X, Y, Val),
    utils:getVal(Coef, X, Y, CoefCell),
    Score is Val * CoefCell.

bestMove(Board, X, Y, Player) :- utils_ai:possibleMoves(Board, Player, PossibleMoves), foundBestMove(Board, PossibleMoves, -99999, X, Y, Player), !.

%foundBestMove(+Board, +MoveList, +BestScore, -BestX, -BestY, +Player)
foundBestMove(_, [], _, _, _, _) :- format(user_output, 'end ai2~n', []), !.

foundBestMove(Board, [Move|Tail], BestScore, BestX, BestY, Player) :-
    utils_ai:getXYMove(Move, X, Y), 
    utils:updateBoard(Board, Player, X, Y, NewBoard), 
    getScoreBoard(NewBoard, Score, Player), 
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
    


    
    
    
    
    
    
    
    
