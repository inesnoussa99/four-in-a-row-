:- begin_tests(profiling).
:- consult(game).
:- consult(ia_minimax).

test(depth3_fast,true(Time < 1.0)) :-
    init_board, board(B),
    get_time(T1),
    ia_minimax(B,_),
    get_time(T2),
    Time is T2-T1.

:- end_tests(profiling).
