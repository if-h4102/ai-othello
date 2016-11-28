:- module(utils, [getVal/4, isCaseEmpty/3, isOnBoard/2, nextCase/4]).
:- use_module('end-of-game', []).

getVal(Board, X, Y, Val)  :- 
  nth0(X, Board, Column), 
  nth0(Y, Column, Val).
  
isCaseEmpty(Board, X, Y) :-
  getVal(Board, X, Y, Val),
  var(Val).

isOnBoard(X, Y) :- 
  X > 0, X < 9, Y > 0, Y < 9.
  
nextCase(X, Y, _, _) :-
  isOnBoard(X, Y),
  X == 8,
  Y == 8,
  !,
  fail.

nextCase(X, Y, NewX, NewY) :-
  isOnBoard(X, Y),
  X == 8,
  NewX is 1,
  NewY is Y + 1.
  
nextCase(X, Y, NewX, NewY) :-
  isOnBoard(X, Y),
  NewX is X + 1,
  NewY is Y.

nextBoard(OldBoard, NewBoard) :-
  nextBoard(OldBoard, NewBoard, 1, 1).
  
nextBoard(OldBoard, NewBoard, X, Y) :-
  getVal(NewBoard, X, Y, NewVal),
  var(NewVal),
  getVal(OldBoard, X, Y, OldVar),
  getVal(NewBoard, X, Y, OldVar),
  nextCase(X, Y, NewX, NewY),
  nextBoard(OldBoard, NewBoard, NewX, NewY).
  
  
updateBoardDirection(_, _, _, _, _, _, 0).

updateBoardDirection(_, _, _, _, _, _, SwappedCaseNumber) :- SwappedCaseNumber < 0.
    
updateBoardDirection(NewBoard, Player, X, Y, DeltaX, DeltaY, SwappedCaseNumber) :- 
    NewX is X + DeltaX,
    NewY is Y + DeltaY,
    getVal(NewBoard,NewX,NewY,Player),
    NewSwappedCaseNumber is SwappedCaseNumber - 1,
	updateBoardDirection(NewBoard, Player, X, Y, DeltaX, DeltaY, NewSwappedCaseNumber).

updateBoard(Board, Player, X, Y, NewBoard) :- 
    NewBoard = [[_ ,_ ,_ ,_ ,_ ,_ ,_ ,_ ,_ ,_ ],
                [_ ,_ ,_ ,_ ,_ ,_ ,_ ,_ ,_ ,_ ],
                [_ ,_ ,_ ,_ ,_ ,_ ,_ ,_ ,_ ,_ ],
                [_ ,_ ,_ ,_ ,_ ,_ ,_ ,_ ,_ ,_ ],
                [_ ,_ ,_ ,_ , _, _,_ ,_ ,_ ,_ ],
                [_ ,_ ,_ ,_ , _, _,_ ,_ ,_ ,_ ],
                [_ ,_ ,_ ,_ ,_ ,_ ,_ ,_ ,_ ,_ ],
                [_ ,_ ,_ ,_ ,_ ,_ ,_ ,_ ,_ ,_ ],
                [_ ,_ ,_ ,_ ,_ ,_ ,_ ,_ ,_ ,_ ],
                [_ ,_ ,_ ,_ ,_ ,_ ,_ ,_ ,_ ,_ ]],
    getVal(NewBoard, X, Y, Player),
    game:swappedCaseDirection(Board, X, Y,-1,-1, Player, SwappedCase1), updateBoardDirection(NewBoard, Player, X, Y,-1,-1, SwappedCase1),
    game:swappedCaseDirection(Board, X, Y,-1, 0, Player, SwappedCase2), updateBoardDirection(NewBoard, Player, X, Y,-1, 0, SwappedCase2),
    game:swappedCaseDirection(Board, X, Y,-1, 1, Player, SwappedCase3), updateBoardDirection(NewBoard, Player, X, Y,-1, 1, SwappedCase3),
    game:swappedCaseDirection(Board, X, Y, 0,-1, Player, SwappedCase4), updateBoardDirection(NewBoard, Player, X, Y, 0,-1, SwappedCase4),
    game:swappedCaseDirection(Board, X, Y, 0, 1, Player, SwappedCase5), updateBoardDirection(NewBoard, Player, X, Y, 0, 1, SwappedCase5),
    game:swappedCaseDirection(Board, X, Y, 1,-1, Player, SwappedCase6), updateBoardDirection(NewBoard, Player, X, Y, 1,-1, SwappedCase6),
    game:swappedCaseDirection(Board, X, Y, 1, 0, Player, SwappedCase7), updateBoardDirection(NewBoard, Player, X, Y, 1, 0, SwappedCase7),
    game:swappedCaseDirection(Board, X, Y, 1, 1, Player, SwappedCase8), updateBoardDirection(NewBoard, Player, X, Y, 1, 1, SwappedCase8),
	nextBoard(Board, NewBoard).