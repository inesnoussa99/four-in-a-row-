:- begin_tests(quit).
:- consult(game).

test(quit,true) :-
    catch(throw(quit_game),quit_game,true).

:- end_tests(quit).
