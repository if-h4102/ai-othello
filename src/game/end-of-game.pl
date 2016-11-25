testBoard(Board) :-  Board = [[_ ,_ ,_ ,_ ,_ ,_ ,_ ,_ ,_ ,_ ],
                              [_ ,_ ,_ ,_ ,_ ,_ ,_ ,_ ,_ ,_ ],
                              [_ ,_ ,_ ,_ ,_ ,_ ,_ ,_ ,_ ,_ ],
                              [_ ,_ ,_ ,_ ,_ ,_ ,_ ,_ ,_ ,_ ],
                              [_ ,_ ,_ ,_ , 1,-1,_ ,_ ,_ ,_ ],
                              [_ ,_ ,_ ,_ ,-1, 1,_ ,_ ,_ ,_ ],
                              [_ ,_ ,_ ,_ ,_ ,_ ,_ ,_ ,_ ,_ ],
                              [_ ,_ ,_ ,_ ,_ ,_ ,_ ,_ ,_ ,_ ],
                              [_ ,_ ,_ ,_ ,_ ,_ ,_ ,_ ,_ ,_ ],
                              [_ ,_ ,_ ,_ ,_ ,_ ,_ ,_ ,_ ,_ ]].

test(Board) :- testBoard(Board), isGameNotFinished(Board, 1).

% Return true is the current player can play, otherwise check if the next player can play
isGameNotFinished(Board, Player) :- playerCanPlay(Board, Player).
isGameNotFinished(Board, Player) :- Player2 is -Player, playerCanPlay(Board, Player2).

% Return true if the game is finished.
% Negation by failure: if the game is not finished yet, thanks to the first line and the '!',
% prolog won't backtrack and see the next line => return false.
% Otherwise, go to the next line
gameOver(Board, Player) :- isGameNotFinished(Board, Player), !, fail.
gameOver(Board, Player) :- true.

% Check if a player can play on the board
playerCanPlay(Board,Player) :- playerCanPlayOnLine(Board,Player,1).
% Explore a given column
playerCanPlayOnColumn(Board,Player,X,Y) :- canBePlayed(Board,X,Y,Player).
playerCanPlayOnColumn(Board,Player,X,Y) :- NewY is Y+1, NewY<9, playerCanPlayOnColumn(Board,Player,X,NewY).
% Explore a given Line
playerCanPlayOnLine(Board,Player,X) :- playerCanPlayOnColumn(Board,Player,X,1).
playerCanPlayOnLine(Board,Player,X) :- NewX is X+1, NewX<9, playerCanPlayOnLine(Board,Player,NewX).


% Check if a player can play on a given case
canBePlayed(Board,X,Y,Player) :- isOnBoard(X,Y), getCase(Board,X,Y,Case), isCaseEmpty(Case), isSwappingCase(Board,X,Y,Player).

% Utility methods
isOnBoard(X,Y) :- X>0,X<9,Y>0,Y<9.
getCase(Board,X,Y,Case) :- isOnBoard(X,Y), nth0(X, Board, Column), nth0(Y, Column, Case).
isCaseEmpty(Case) :- var(Case).
% isCaseEmpty(Case) :- Case == 0.

% SwappedCase is positive if case are swapped and negative or zero else.
% if the number is positive it's the number of swapped case in the given direction.
% if the number is negative it's the number of the other player case in the given direction before the first empty case minus 10.
swappedCaseDirection(Board,Xinit,Yinit,DeltaX,DeltaY,Player,SwappedCase) :-
    X is Xinit+DeltaX,
    Y is Yinit+DeltaY,
    getCase(Board,X,Y,Case),
    Case == Player,
    SwappedCase is 0,
    !.

% If this doesn't swapp anything return -10
swappedCaseDirection(Board,Xinit,Yinit,DeltaX,DeltaY,_ ,SwappedCase) :-
    X is Xinit+DeltaX,
    Y is Yinit+DeltaY,
    getCase(Board,X,Y,Case),
    isCaseEmpty(Case),
    SwappedCase is -10,
    !.

swappedCaseDirection(Board,Xinit,Yinit,DeltaX,DeltaY,Player,SwappedCase) :-
    X is Xinit+DeltaX,
    Y is Yinit+DeltaY,
    isOnBoard(X,Y),
    swappedCaseDirection(Board,X,Y,DeltaX,DeltaY,Player,NewSwappedCase),
    SwappedCase is NewSwappedCase + 1.

%% Check if playing a case swapp any case.
isSwappingCase(Board,X,Y,Player) :- swappedCaseDirection(Board,X,Y,1,1,Player,SwappedCase), SwappedCase > 0.
isSwappingCase(Board,X,Y,Player) :- swappedCaseDirection(Board,X,Y,1,0,Player,SwappedCase), SwappedCase > 0.
isSwappingCase(Board,X,Y,Player) :- swappedCaseDirection(Board,X,Y,1,-1,Player,SwappedCase), SwappedCase > 0.
isSwappingCase(Board,X,Y,Player) :- swappedCaseDirection(Board,X,Y,0,1,Player,SwappedCase), SwappedCase > 0.
isSwappingCase(Board,X,Y,Player) :- swappedCaseDirection(Board,X,Y,0,-1,Player,SwappedCase), SwappedCase > 0.
isSwappingCase(Board,X,Y,Player) :- swappedCaseDirection(Board,X,Y,-1,1,Player,SwappedCase), SwappedCase > 0.
isSwappingCase(Board,X,Y,Player) :- swappedCaseDirection(Board,X,Y,-1,0,Player,SwappedCase), SwappedCase > 0.
isSwappingCase(Board,X,Y,Player) :- swappedCaseDirection(Board,X,Y,-1,-1,Player,SwappedCase), SwappedCase > 0.


%%%% Recursive predicate for playing the game.
% The game is over, we use a cut to stop the proof search, and display the winner/board.
% play(_):- gameOver(Board, Player), !, write('Game is Over. Winner: '), writeln(Winner), displayBoard.
% The game is not over, we play the next turn
% play(Player):-  write('New turn for:'), writeln(Player),
%         board(Board), % instanciate the board from the knowledge base
%             displayBoard, % print it
%             ia(Board, Move,Player), % ask the AI for a move, that is, an index for the Player
%           playMove(Board,Move,NewBoard,Player), % Play the move and get the result in a new Board
%         applyIt(Board, NewBoard), % Remove the old board from the KB and store the new one
%           changePlayer(Player,NextPlayer), % Change the player before next turn
%            play(NextPlayer). % next turn!
