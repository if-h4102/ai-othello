
board(Board) :- Board = [[_,_,_,_,_,_,_,_,_,_],
                          [_,1,1,1,1,1,1,1,1,_],
                          [_,_,_,_,_,_,_,_,_,_],
                          [_,-1,-1,-1,-1,-1,-1,-1,-1,_],
                          [_,_,_,_,_,_,_,_,_,_],
                          [_,_,_,_,_,_,_,_,_,_],
                          [_,_,_,_,_,_,_,_,_,_],
                          [_,_,-1,_,_,_,_,-1,_,_],
                          [_,1,_,_,_,_,_,_,1,_],
                          [_,_,_,_,_,_,_,_,_,_]].


getVal(IndexRow, IndexColumn, Val)  :- board(Board),  nth0(IndexRow, Board, Column), nth0(IndexColumn, Column, Val).

displayCell(Val) :- var(Val), write('   |'), !.
displayCell(-1) :- write(' x |'), !.
displayCell(1) :- write(' o |'), !.

displayCell(IndexRow, IndexColumn) :- getVal(IndexRow, IndexColumn, Val), displayCell(Val).

displayRow(IndexRow) :- 
    write('|'), 
    displayCell(IndexRow, 1),
    displayCell(IndexRow, 2),
    displayCell(IndexRow, 3),
    displayCell(IndexRow, 4),
    displayCell(IndexRow, 5),
    displayCell(IndexRow, 6),
    displayCell(IndexRow, 7),
    displayCell(IndexRow, 8),
    writeln(''),
 	writeln('---------------------------------').

displayBoard:- 
	writeln('---------------------------------'),
    displayRow(1),
    displayRow(2),
    displayRow(3),
    displayRow(4),
    displayRow(5),
    displayRow(6),
    displayRow(7),
    displayRow(8),
    writeln('').
    