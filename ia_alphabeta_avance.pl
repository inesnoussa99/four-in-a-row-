% =========================
% File: ia_alphabeta_avance.pl
% (Iterative Deepening + AlphaBeta)
% =========================

:- [game].
:- use_module(library(lists)).

time_budget_ms(800).

% ---------- WRAPPER (mêmes règles que ton game.pl)
abid_valid_moves(Board, Moves) :-
    findall(Col,
        ( between(1, 7, Col),
          valid_col(Board, Col)
        ),
        Moves).

abid_apply_move(Board, Player, Col, NewBoard) :-
    playMove(Board, Col, NewBoard, Player).

abid_opponent(Player, Opp) :-
    changePlayer(Player, Opp).

abid_is_win(BoardAfterMove) :- gameover(BoardAfterMove).
abid_is_draw(Board) :- board_full(Board).

% =========================================================
% API:
%   ia_alphabeta_avance(+Board, +Player, -BestCol)
% =========================================================

ia_alphabeta_avance(Board, Player, BestCol) :-
    get_time(T0),
    time_budget_ms(Budget),
    Deadline is T0 + Budget / 1000.0,
    abid_valid_moves(Board, Moves),
    abid_order_moves_center_first(Moves, Ordered),
    abid_iterative_deepen(1, 20, Deadline, Ordered, Board, Player, none, BestCol).

abid_iterative_deepen(D, DMax, Deadline, Moves, Board, Player, BestSoFar, BestOut) :-
    get_time(Now),
    ( Now >= Deadline ->
        abid_pick_fallback(Moves, BestSoFar, BestOut)
    ; D > DMax ->
        abid_pick_fallback(Moves, BestSoFar, BestOut)
    ;   ( catch(abid_best_at_depth(Moves, Board, Player, D, BestD), _, fail) ->
            Best1 = BestD
        ;   Best1 = BestSoFar
        ),
        D1 is D + 1,
        abid_iterative_deepen(D1, DMax, Deadline, Moves, Board, Player, Best1, BestOut)
    ).

abid_pick_fallback(Moves, none, Best) :- !, Moves = [Best|_].
abid_pick_fallback(_Moves, Best, Best).

abid_best_at_depth(Moves, Board, Player, D, BestCol) :-
    abid_alphabeta_root(Moves, Board, Player, D, -1000000, 1000000, none, BestCol, _).

% -------- AlphaBeta (préfixe abid_)
abid_alphabeta_root([], _Board, _Player, _D, _A, _B, BestSoFar, BestSoFar, -1000000).
abid_alphabeta_root([Col|Cols], Board, Player, D, A, B, BestSoFar, BestCol, BestScore) :-
    abid_apply_move(Board, Player, Col, B2),

    ( abid_is_win(B2) ->
        Score is 100000
    ; abid_is_draw(B2) ->
        Score is 0
    ; D > 1 ->
        abid_opponent(Player, Opp),
        D1 is D - 1,
        abid_alphabeta(B2, Opp, D1, -B, -A, ScoreNeg),
        Score is -ScoreNeg
    ;   abid_evaluate(B2, Score)
    ),

    ( Score > A ->
        A1 = Score, Best1 = Col, BestScore1 = Score
    ;   A1 = A, Best1 = BestSoFar, BestScore1 = BestScore
    ),
    ( A1 >= B ->
        BestCol = Best1, BestScore = BestScore1
    ;   abid_alphabeta_root(Cols, Board, Player, D, A1, B, Best1, BestCol, BestScore)
    ).

abid_alphabeta(Board, _Player, _D, _A, _B, 0) :-
    abid_is_draw(Board), !.
abid_alphabeta(Board, _Player, 0, _A, _B, Score) :- !,
    abid_evaluate(Board, Score).

abid_alphabeta(Board, Player, D, A, B, BestScore) :-
    abid_valid_moves(Board, Moves),
    ( Moves = [] ->
        BestScore = 0
    ;   abid_order_moves_center_first(Moves, Ordered),
        abid_opponent(Player, Opp),
        D1 is D - 1,
        abid_ab_loop(Ordered, Board, Player, Opp, D1, A, B, -1000000, BestScore)
    ).

abid_ab_loop([], _Board, _P, _Opp, _D1, _A, _B, Best, Best).
abid_ab_loop([Col|Cols], Board, Player, Opp, D1, A, B, BestSoFar, BestOut) :-
    abid_apply_move(Board, Player, Col, B2),

    ( abid_is_win(B2) ->
        Score is 100000
    ; abid_is_draw(B2) ->
        Score is 0
    ;   abid_alphabeta(B2, Opp, D1, -B, -A, ScoreNeg),
        Score is -ScoreNeg
    ),

    Best1 is max(BestSoFar, Score),
    A1 is max(A, Score),
    ( A1 >= B ->
        BestOut = Best1
    ;   abid_ab_loop(Cols, Board, Player, Opp, D1, A1, B, Best1, BestOut)
    ).

abid_order_moves_center_first(Moves, Ordered) :-
    map_list_to_pairs(abid_center_dist, Moves, Pairs),
    keysort(Pairs, Sorted),
    pairs_values(Sorted, Ordered).

abid_center_dist(Col, Dist) :-
    Dist is abs(Col - 4).

abid_evaluate(_Board, 0).
