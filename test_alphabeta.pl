:- begin_tests(alphabeta).

:- consult(game).
:- consult(ia_alphabeta).

test(alpha_center,true) :-
    init_board,
    board(B),
    ia_alphabeta:ia(B,C,x),
    between(3,5,C).

test(alpha_block_horizontal,true) :-
    Board = [
        [o,o,o],[],[],[],[],[],[]
    ],
    ia_alphabeta:ia(Board,Col,x),
    Col =:= 1.

:- end_tests(alphabeta).
