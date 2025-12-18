:- begin_tests(game).
:- consult(game).

test(init, true) :-
    init_board,
    board(B),
    length(B, 6), !.

test(valid_column, true) :-
    init_board,
    board(B),
    valid_col(B,3), !.

test(play_move, true) :-
    init_board,
    board(B),
    playMove(B,3,N,'x'),
    column(N,3,Col),
    last(Col,'x'), !.

test(change_player, true) :-
    changePlayer('x','o'),
    changePlayer('o','x'), !.

:- end_tests(game).
