:- begin_tests(benchmark).

:- consult(game).
:- consult(ia_v2).
:- consult(ia_minimax).

test(v2_vs_minimax,true) :-
    init_board,
    board(B),
    ia_v2(B,x,C1),
    ia_minimax(B,C2),
    between(1,7,C1),
    between(1,7,C2).

:- end_tests(benchmark).
