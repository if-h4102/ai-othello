:- module('utils', [getVal/4, isCaseEmpty/3, isOnBoard/2, nextCase/4]).
:- use_module('end-of-game', []).

% getVal(+Board, +X, +Y, ?Val)
% If Val is instanciated, it becomes the value of the cell (X, Y).
% If Val is instanciated and the cell (X, Y) is empty, the cell (X, Y) becomes the value of Val.
% If Val is instanciated and the cell (X, Y) is not empty, this method could returns false.
getVal(Board, X, Y, Val)  :- 
  nth0(X, Board, Column), 
  nth0(Y, Column, Val).
  
% isCaseEmpty(+Board, +X, +Y)
% returns true if the cell (X, Y) is empty, false otherwise.
isCaseEmpty(Board, X, Y) :-
  getVal(Board, X, Y, Val),
  var(Val).

% isOnBoard(?X, ?Y)
% If X and Y are instanciated, returns true if the cell (X, Y) is on the board.
% If either X or Y or both are not instanciated, returns a possible cell of the board.
isOnBoard(X, Y) :- 
  between(1, 8, X),
  between(1, 8, Y).
  
% nextCase(+X, +Y, -NewX, -NewY)
% Returns the next cell of the board, after the cell (X, Y).
% If the cell is the cell (8, 8), returns false (no other cell available).

% Returns false if the current cell is (8, 8).
nextCase(X, Y, _, _) :-
  isOnBoard(X, Y),
  X == 8,
  Y == 8,
  !,
  fail.

% Returns the first cell of the following row.
nextCase(X, Y, NewX, NewY) :-
  isOnBoard(X, Y),
  X == 8,
  NewX is 1,
  NewY is Y + 1.
  
% Returns the next cell in the row.
nextCase(X, Y, NewX, NewY) :-
  isOnBoard(X, Y),
  NewX is X + 1,
  NewY is Y.

% nextBoard(+OldBoard, +NewBoard)
% For all the non instantiated cell in NewBoard, put the value of the same cell of OldBoard in NewBoard (value potentially not instantiated).
nextBoard(OldBoard, NewBoard) :-
  nextBoard(OldBoard, NewBoard, 1, 1), !.
  
% nextBoard(+OldBoard, +NewBoard, +X, +Y)
% Get the value of the cell (X, Y) of OldBoard, and check if this val is not instantiated.
% If yes, put the value of the cell (X, Y) of OldBoard in the cell (X, Y) of NewBoard, and calls for the next cell.
nextBoard(OldBoard, NewBoard, X, Y) :-
  isOnBoard(X,Y),
  getVal(NewBoard, X, Y, NewVal),
  var(NewVal),
  getVal(OldBoard, X, Y, OldVar),
  getVal(NewBoard, X, Y, OldVar),
  nextCase(X, Y, NewX, NewY),
  nextBoard(OldBoard, NewBoard, NewX, NewY), !.
  
% If the value of the cell (X, Y) of OldBoard is instantiated, go to the next cell.
nextBoard(OldBoard, NewBoard, X, Y) :-
  isOnBoard(X,Y),
  nextCase(X, Y, NewX, NewY),
  nextBoard(OldBoard, NewBoard, NewX, NewY), !.
  
% End of the method nextBoard.
nextBoard( _, _, _, _).
  
% updateBoardDirection(+NewBoard, +Player, +X, +Y, +DeltaX, +DeltaY, +SwappedCaseNumber)
% Put SwappedCaseNumber cell from the cell (X + DeltaX, Y + DeltaY) and in the direction defined by DeltaX and DeltaY to the value of Player.
% The new value is in NewBoard.
% All the cells that will be changed by this method must be empty (not instanciated) in NewBoard.

% If the number of case to swapped is 0, do nothing.
updateBoardDirection(_, _, _, _, _, _, 0) :- !.

% If the number of case to swapped is less than 0, do nothing.
updateBoardDirection(_, _, _, _, _, _, SwappedCaseNumber) :- SwappedCaseNumber < 0, !.
    
% Put the cell (X + DeltaX, Y + DeltaY) in NewBoard to the value of Player.
% Recursively call itself while SwappedCaseNumber is positive.
updateBoardDirection(NewBoard, Player, X, Y, DeltaX, DeltaY, SwappedCaseNumber) :- 
    NewX is X + DeltaX,
    NewY is Y + DeltaY,
    getVal(NewBoard,NewX,NewY,Player),
    NewSwappedCaseNumber is SwappedCaseNumber - 1,
    updateBoardDirection(NewBoard, Player, NewX, NewY, DeltaX, DeltaY, NewSwappedCaseNumber),
    !.

% updateBoard(+Board, +Player, +X, +Y, -NewBoard).
% Make all change when Player plays in the cell (X, Y), and returns the new board in NewBoard.
% The validity of the move is not checked in this method.
updateBoard(Board, Player, X, Y, NewBoard) :- 
    % Create the new board.
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
    % Put the cell (X, Y) to Player
    getVal(NewBoard, X, Y, Player),
    % Swap all necessary cell to Player
    end_of_game:swappedCaseDirection(Board, X, Y,-1,-1, Player, SwappedCase1), updateBoardDirection(NewBoard, Player, X, Y,-1,-1, SwappedCase1),
    end_of_game:swappedCaseDirection(Board, X, Y,-1, 0, Player, SwappedCase2), updateBoardDirection(NewBoard, Player, X, Y,-1, 0, SwappedCase2),
    end_of_game:swappedCaseDirection(Board, X, Y,-1, 1, Player, SwappedCase3), updateBoardDirection(NewBoard, Player, X, Y,-1, 1, SwappedCase3),
    end_of_game:swappedCaseDirection(Board, X, Y, 0,-1, Player, SwappedCase4), updateBoardDirection(NewBoard, Player, X, Y, 0,-1, SwappedCase4),
    end_of_game:swappedCaseDirection(Board, X, Y, 0, 1, Player, SwappedCase5), updateBoardDirection(NewBoard, Player, X, Y, 0, 1, SwappedCase5),
    end_of_game:swappedCaseDirection(Board, X, Y, 1,-1, Player, SwappedCase6), updateBoardDirection(NewBoard, Player, X, Y, 1,-1, SwappedCase6),
    end_of_game:swappedCaseDirection(Board, X, Y, 1, 0, Player, SwappedCase7), updateBoardDirection(NewBoard, Player, X, Y, 1, 0, SwappedCase7),
    end_of_game:swappedCaseDirection(Board, X, Y, 1, 1, Player, SwappedCase8), updateBoardDirection(NewBoard, Player, X, Y, 1, 1, SwappedCase8),
    % Copie all the cell not changed by the move from OldBoard to NewBoard.
    nextBoard(Board, NewBoard),
    !.
