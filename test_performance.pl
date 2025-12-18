:- begin_tests(performance).

:- set_prolog_flag(testing, true).

:- consult(game).
:- consult(ia_minimax).

time_call(Goal,Time) :-
    get_time(T1),
    call(Goal),
    get_time(T2),
    Time is T2 - T1.

test(minimax_speed, true(Time < 0.8)) :-
    init_board,
    board(B),
    asserta(minimax_depth(3)),
    time_call(ia_minimax(B,_), Time),
    retract(minimax_depth(3)), !.

test(minimax_valid, true) :-
    init_board,
    board(B),
    asserta(minimax_depth(3)),
    ia_minimax(B,Col),
    retract(minimax_depth(3)),
    between(1,7,Col), !.

:- end_tests(performance).
