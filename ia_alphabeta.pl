% =========================
% File: ia_alphabeta.pl
% IA alpha-beta (Negamax) avec heuristique (même idée que ia_minimax)
% Compatible avec TON game.pl
% =========================

:- [game].
:- use_module(library(pairs)).   % pairs_values/2

% Profondeur par défaut
search_depth(6).

% =========================================================
% WRAPPERS compatibles avec ton game.pl
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

is_draw(Board) :-
    board_full(Board).

% IMPORTANT : victoire du joueur P (et pas juste "gameover/1")
is_win(Board, P) :-
    win_player(Board, P).

% =========================================================
% API attendue :
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

    ( is_win(B2, Player) ->
        Score is 100000
    ; is_draw(B2) ->
        Score is 0
    ; D > 1 ->
        opponent(Player, Opp),
        D1 is D - 1,
        alphabeta(B2, Opp, D1, -B, -A, ScoreNeg),
        Score is -ScoreNeg
    ;   evaluate(B2, Player, Score)   % <-- heuristique branchée ici
    ),

    ( Score > A ->
        A1 = Score, Best1 = Col, BestScore1 = Score
    ;   A1 = A, Best1 = BestSoFar, BestScore1 = BestScore
    ),
    ( A1 >= B ->
        BestCol = Best1, BestScore = BestScore1
    ;   alphabeta_root(Cols, Board, Player, D, A1, B, Best1, BestCol, BestScore)
    ).

alphabeta(Board, _Player, _D, _A, _B, 0) :-
    is_draw(Board), !.

alphabeta(Board, Player, 0, _A, _B, Score) :- !,
    evaluate(Board, Player, Score).

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

    ( is_win(B2, Player) ->
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
% HEURISTIQUE (même logique que ia_minimax, mais "générique joueur")
% Score positif = bon pour Player, négatif = bon pour adversaire
% =========================================================

evaluate(Board, Player, Score) :-
    opponent(Player, Opp),
    heuristic_for(Board, Player, SP),
    heuristic_for(Board, Opp, SO),
    Score is SP - SO.

heuristic_for(Board, P, Score) :-
    findall(S, score_lines_for(Board, P, S), Scores),
    sum_list_local(Scores, Score).

score_lines_for(Board, P, Score) :-
    ( get_line_h(Board, Line)
    ; get_line_v(Board, Line)
    ; get_line_d(Board, Line)
    ),
    score_4_cells_for(Line, P, Score).

% Fenêtre de 4 cases, score du point de vue de P
score_4_cells_for(List4, P, Score) :-
    opponent(P, Opp),
    count_occ(P,   List4, CP),
    count_occ(Opp, List4, CO),
    count_occ('.', List4, CE),
    ( CO > 0, CP > 0 ->
        Score = 0          % fenêtre "bloquée" (mix)
    ; CP =:= 4 ->
        Score = 1000       % victoire (théorique)
    ; CO =:= 4 ->
        Score = -1000
    ; CP =:= 3, CE =:= 1 ->
        Score = 50
    ; CP =:= 2, CE =:= 2 ->
        Score = 10
    ; CO =:= 3, CE =:= 1 ->
        Score = -50
    ; CO =:= 2, CE =:= 2 ->
        Score = -10
    ;   Score = 0
    ).

count_occ(X, L, N) :- include(==(X), L, R), length(R, N).

sum_list_local([], 0).
sum_list_local([H|T], S) :-
    sum_list_local(T, S1),
    S is H + S1.

% =========================================================
% Génération des fenêtres de 4 (comme dans ia_minimax)
% =========================================================

get_line_h(Board, [A,B,C,D]) :-
    member(Row, Board),
    append(_, [A,B,C,D|_], Row).

get_line_v(Board, [A,B,C,D]) :-
    column(Board, _, ColList),
    append(_, [A,B,C,D|_], ColList).

get_line_d(Board, Diag) :-
    ( win_d1_gen(Board, Diag)
    ; win_d2_gen(Board, Diag)
    ).

win_d1_gen(Board, [A,B,C,D]) :-
    between(1,3,Row),
    between(1,4,Col),
    diag_down(Board, Row, Col, [A,B,C,D]).

win_d2_gen(Board, [A,B,C,D]) :-
    between(4,6,Row),
    between(1,4,Col),
    diag_up(Board, Row, Col, [A,B,C,D]).

% =========================================================
% Détection de victoire (copie de ia_minimax)
% =========================================================

win_player(Board, Player) :-
    ( win_h_p(Board, Player)
    ; win_v_p(Board, Player)
    ; win_d1_p(Board, Player)
    ; win_d2_p(Board, Player)
    ), !.

win_h_p(Board, P) :-
    member(Row, Board),
    append(_, [P,P,P,P|_], Row).

win_v_p(Board, P) :-
    between(1,7,C),
    column(Board, C, Col),
    append(_, [P,P,P,P|_], Col).

win_d1_p(Board, P) :-
    win_d1_gen(Board, [P,P,P,P]).

win_d2_p(Board, P) :-
    win_d2_gen(Board, [P,P,P,P]).
