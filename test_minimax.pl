:- begin_tests(minimax).
:- consult(game).
:- consult(ia_minimax).

test(minimax_center_empty,true) :-
    init_board, board(B),
    ia_minimax(B,C),
    between(3,5,C), !.

test(minimax_block_vertical,true) :-
    Board = [
        [],[],[o,o,o],[],[],[],[]
    ],
    ia_minimax(Board,Col),
    Col =:= 3, !.

test(minimax_win_now,true) :-
    Board = [
        [x,x,x],[],[],[],[],[],[]
    ],
    ia_minimax(Board,Col),
    Col =:= 1, !.

:- end_tests(minimax).
