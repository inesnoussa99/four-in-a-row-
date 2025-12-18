:- begin_tests(syntax).

test(all_files_load) :-
    consult(game),
    consult(ia_v1),
    consult(ia_v2),
    consult(ia_minimax),
    consult(ia_alphabeta),
    consult(main).

:- end_tests(syntax).
