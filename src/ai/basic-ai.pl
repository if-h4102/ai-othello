


getScoreBoard(Board,Score) :- getScoreBoard(Board,Score,1,1).

getScoreBoard(Board,Score,8,9) :- Score is 0.
%% if we try to go too far on Y increment X
getScoreBoard(Board,Score,LastX,9) :- X is LastX+1, getScoreBoard(Board,Score,X,1).
%% if the case is unset, score don't change
getScoreBoard(Board,Score,LastX,LastY) :- getCase(Board,LastX,LastY,Case), var(Case), Y is LastY+1, getScoreBoard(Board, Score, LastX, Y).
%% else add the case value to the currant score
getScoreBoard(Board,Score,LastX,LastY) :- getCase(Board,LastX,LastY,Case), Y is LastY+1, getScoreBoard(Board, OldScore, LastX, Y), Score is OldScore+Case. 

