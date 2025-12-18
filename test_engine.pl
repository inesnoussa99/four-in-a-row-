:- begin_tests(engine).

:- consult(game).
:- consult(ia_v2).
:- consult(ia_minimax).

test(v2_runs, true) :-
    init_board,
    board(B),
    ia_v2(B,'x',Col),
    between(1,7,Col), !.

test(minimax_runs, true) :-
    init_board,
    board(B),
    ia_minimax(B,Col),
    between(1,7,Col), !.

:- end_tests(engine).
