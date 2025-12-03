:- dynamic board/1.

applyIt(Old,New) :- retract(board(Old)), assert(board(New)).

displayBoard :-
    board(B),
    writeln(B).

gameover(B,M) :-
    (between(1,3,M), nth1(1,B,V),nth1(2,B,V),nth1(3,B,V), V \= '.');
    (between(4,6,M), nth1(4,B,V),nth1(5,B,V),nth1(6,B,V), V \= '.');
    (between(7,9,M), nth1(7,B,V),nth1(8,B,V),nth1(9,B,V), V \= '.');
    ((M=1;M=4;M=7), nth1(1,B,V),nth1(4,B,V),nth1(7,B,V), V \= '.');
    ((M=2;M=5;M=8), nth1(2,B,V),nth1(5,B,V),nth1(8,B,V), V \= '.');
    ((M=3;M=6;M=9), nth1(3,B,V),nth1(6,B,V),nth1(9,B,V), V \= '.');
    ((M=1;M=5;M=9), nth1(1,B,V),nth1(5,B,V),nth1(9,B,V), V \= '.');
    ((M=3;M=5;M=7), nth1(3,B,V),nth1(5,B,V),nth1(7,B,V), V \= '.').

ia(B,M,_) :-
    repeat,
    random_between(1,10,M),
    nth1(M,B,X),
    (X='.' -> ! ; fail).

replace([_|T], 1, X, [X|T]).
replace([H|T], I, X, [H|R]) :- I > 1, I1 is I-1, replace(T, I1, X, R).

playMove(Board,M,NewBoard,Player) :-
    replace(Board, M, Player, NewBoard).

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
    length(Board,9),
    Board = ['.','.','.','.','.','.','.','.','.'],
    assert(board(Board)),
    play('x').