:- dynamic board/1.

applyIt(Old,New) :- retract(board(Old)), assert(board(New)).

displayBoard :-
    board(B),
    writeln(B).

gameover(B,M) :- true.

ia(B,M,_) :- true.

playMove(Board,M,NewBoard,Player) :- true.

changePlayer(P,N) :- (P='x',N='o');(P='o',N='x').

play(Player):-
    write('New turn for: '), writeln(Player),
    board(Board),
    displayBoard,
    
    ia(Board, Move, Player),
    
    playMove(Board, Move, NewBoard, Player),
    applyIt(Board, NewBoard),
    
    ( gameover(NewBoard, Move) ->
        displayBoard, writeln(Player), writeln(' wins!'), retractall(board(_))
    ; \+ member('.', NewBoard) ->
        displayBoard, writeln('Draw!'), retractall(board(_))
    ;
        changePlayer(Player,NextPlayer),
        play(NextPlayer)
    ).

init :-
    retractall(board(_)),
    length(Board,6),
    Board = [['.','.','.','.','.','.','.'],

             ['.','.','.','.','.','.','.'],

             ['.','.','.','.','.','.','.'],

             ['.','.','.','.','.','.','.'],

             ['.','.','.','.','.','.','.'],

             ['.','.','.','.','.','.','.']],

    assert(board(Board)),
    play('x').