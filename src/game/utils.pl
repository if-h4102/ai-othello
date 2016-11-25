:- module(utils, [getVal/3, isCaseEmpty/2, isOnBoard/2, nextCase/4]).

getVal(X, Y, Val)  :- 
  board(Board),  
  nth0(X, Board, Column), 
  nth0(Y, Column, Val).
  
isCaseEmpty(X, Y) :-
  getVal(X, Y, Val),
  var(Val).

isOnBoard(X,Y) :- 
  X > 0, X < 9, Y > 0, Y < 9.
  
nextCase(X, Y, NewX, NewY) :-
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
  
% Private Method
getVal(X, Y, Board, Val)  :-  
  nth0(X, Board, Column), 
  nth0(Y, Column, Val).
  
nextBoard(OldBoard, NewBoard, X, Y) :-
  getVal(X, Y, NewBoard, NewVal),
  var(NewVal),
  getVal(X, Y, OldBoard, OldVar),
  getVal(X, Y, NewBoard, OldVar),
  nextCase(X, Y, NewX, NewY),
  nextBoard(OldBoard, NewBoard, NewX, NewY).
  
  
updateBoardDirection(_, _, _, _, _, _, 0).

updateBoardDirection(_, _, _, _, _, _, SwappedCaseNumber) :- SwappedCaseNumber < 0.
    
updateBoardDirection(NewBoard, Player, X, Y, DeltaX, DeltaY, SwappedCaseNumber) :- 
    NewX is X + DeltaX,
    NewY is Y + DeltaY,
    getCase(NewBoard,NewX,NewY,Player),
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
    getCase(NewBoard, X, Y, Player),
    swappedCaseDirection(Board, X, Y,-1,-1, Player, SwappedCase1), updateBoardDirection(NewBoard, Player, X, Y,-1,-1, SwappedCase1),
    swappedCaseDirection(Board, X, Y,-1, 0, Player, SwappedCase2), updateBoardDirection(NewBoard, Player, X, Y,-1, 0, SwappedCase2),
    swappedCaseDirection(Board, X, Y,-1, 1, Player, SwappedCase3), updateBoardDirection(NewBoard, Player, X, Y,-1, 1, SwappedCase3),
    swappedCaseDirection(Board, X, Y, 0,-1, Player, SwappedCase4), updateBoardDirection(NewBoard, Player, X, Y, 0,-1, SwappedCase4),
    swappedCaseDirection(Board, X, Y, 0, 1, Player, SwappedCase5), updateBoardDirection(NewBoard, Player, X, Y, 0, 1, SwappedCase5),
    swappedCaseDirection(Board, X, Y, 1,-1, Player, SwappedCase6), updateBoardDirection(NewBoard, Player, X, Y, 1,-1, SwappedCase6),
    swappedCaseDirection(Board, X, Y, 1, 0, Player, SwappedCase7), updateBoardDirection(NewBoard, Player, X, Y, 1, 0, SwappedCase7),
    swappedCaseDirection(Board, X, Y, 1, 1, Player, SwappedCase8), updateBoardDirection(NewBoard, Player, X, Y, 1, 1, SwappedCase8),
	nextBoard(Board, NewBoard).