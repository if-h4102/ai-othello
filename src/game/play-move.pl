:- module(play-move, [playMove/4]).
:- use_module(gameutils, []).


playMove(X, Y, Player, NextBoard) :-
  getVal(X, Y, Player),

  
swappedCaseDirection(Board,Xinit,Yinit,DeltaX,DeltaY,Player) :-
    X is Xinit+DeltaX,
    Y is Yinit+DeltaY,
    getVal(X,Y,Case),
    Case == Player,
    SwappedCase is 0,
    !.

% If this doesn't swapp anything return -10
swappedCaseDirection(Board,Xinit,Yinit,DeltaX,DeltaY,_) :-
    X is Xinit+DeltaX,
    Y is Yinit+DeltaY,
    getVal(X,Y,Case),
    isCaseEmpty(Case),
    SwappedCase is -10,
    !.

swappedCaseDirection(Board,Xinit,Yinit,DeltaX,DeltaY,Player) :-
    X is Xinit+DeltaX,
    Y is Yinit+DeltaY,
    isOnBoard(X,Y),
    swappedCaseDirection(Board,X,Y,DeltaX,DeltaY,Player,NewSwappedCase),
    SwappedCase is NewSwappedCase + 1.