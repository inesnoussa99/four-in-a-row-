% =========================
% File: ia_alphabeta.pl
% =========================

:- [game].
:- use_module(library(lists)).

% Profondeur par défaut
search_depth(8).

% =========================================================
% WRAPPER compatible avec TON game.pl
% =========================================================

valid_moves(Board, Moves) :-
    findall(Col,
        ( between(1, 7, Col),
          valid_col(Board, Col)
        ),
        Moves).

apply_move(Board, Player, Col, NewBoard) :-
    playMove(Board, Col, NewBoard, Player).

opponent(Player, Opp) :-
    changePlayer(Player, Opp).

% terminal win si le joueur qui vient de jouer a gagné
is_win(BoardAfterMove) :-
    gameover(BoardAfterMove).

is_draw(Board) :-
    board_full(Board).

% =========================================================
% API attendue par le main :
%   ia_alphabeta(+Board, +Player, -BestCol)
% =========================================================

ia_alphabeta(Board, Player, BestCol) :-
    search_depth(D),
    valid_moves(Board, Moves),
    order_moves_center_first(Moves, Ordered),
    alphabeta_root(Ordered, Board, Player, D, -1000000, 1000000, none, BestCol, _BestScore).

% =========================================================
% Alpha-Beta (Negamax)
% =========================================================

alphabeta_root([], _Board, _Player, _D, _A, _B, BestSoFar, BestSoFar, -1000000).
alphabeta_root([Col|Cols], Board, Player, D, A, B, BestSoFar, BestCol, BestScore) :-
    apply_move(Board, Player, Col, B2),

    % si ce coup gagne immédiatement, on renvoie un score énorme
    ( is_win(B2) ->
        Score is 100000
    ; is_draw(B2) ->
        Score is 0
    ; D > 1 ->
        opponent(Player, Opp),
        D1 is D - 1,
        alphabeta(B2, Opp, D1, -B, -A, ScoreNeg),
        Score is -ScoreNeg
    ;   evaluate(B2, Score)
    ),

    ( Score > A ->
        A1 = Score, Best1 = Col, BestScore1 = Score
    ;   A1 = A, Best1 = BestSoFar, BestScore1 = BestScore
    ),
    ( A1 >= B ->
        BestCol = Best1, BestScore = BestScore1
    ;   alphabeta_root(Cols, Board, Player, D, A1, B, Best1, BestCol, BestScore)
    ).

alphabeta(Board, _Player, _D, _A, _B, Score) :-
    is_draw(Board), !,
    Score is 0.

alphabeta(Board, _Player, 0, _A, _B, Score) :- !,
    evaluate(Board, Score).

alphabeta(Board, Player, D, A, B, BestScore) :-
    valid_moves(Board, Moves),
    ( Moves = [] ->
        BestScore = 0
    ;   order_moves_center_first(Moves, Ordered),
        opponent(Player, Opp),
        D1 is D - 1,
        ab_loop(Ordered, Board, Player, Opp, D1, A, B, -1000000, BestScore)
    ).

ab_loop([], _Board, _P, _Opp, _D1, _A, _B, Best, Best).
ab_loop([Col|Cols], Board, Player, Opp, D1, A, B, BestSoFar, BestOut) :-
    apply_move(Board, Player, Col, B2),

    ( is_win(B2) ->
        Score is 100000
    ; is_draw(B2) ->
        Score is 0
    ;   alphabeta(B2, Opp, D1, -B, -A, ScoreNeg),
        Score is -ScoreNeg
    ),

    Best1 is max(BestSoFar, Score),
    A1 is max(A, Score),
    ( A1 >= B ->
        BestOut = Best1
    ;   ab_loop(Cols, Board, Player, Opp, D1, A1, B, Best1, BestOut)
    ).

% =========================================================
% Move ordering : centre d'abord (colonnes 1..7)
% =========================================================

order_moves_center_first(Moves, Ordered) :-
    map_list_to_pairs(center_dist, Moves, Pairs),
    keysort(Pairs, Sorted),
    pairs_values(Sorted, Ordered).

center_dist(Col, Dist) :-
    Dist is abs(Col - 4).

% =========================================================
% Evaluation (placeholder)
% -> tu peux brancher ton heuristique minimax ici
% =========================================================

evaluate(_Board, 0).
