% =========================
% File: profiling.pl
% Outils de comptage de nœuds explorés et pruning
% =========================

:- module(profiling, [
    reset_profile/0,
    profile_minimax/4,        % profile_minimax(+BoardOrVar,+Player,+Depth,-profile(BestCol,Nodes))
    profile_alphabeta/5       % profile_alphabeta(+BoardOrVar,+Player,+Depth,+ABBound,-profile(BestCol,Nodes,Prunes))
]).

:- [game].
:- use_module(library(pairs)).   % pairs_values/2
:- use_module(library(apply)).   % maplist/3

:- dynamic node_count/1.
:- dynamic prune_count/1.

% -------------------------
% Counters
% -------------------------
reset_profile :-
    retractall(node_count(_)),
    retractall(prune_count(_)),
    asserta(node_count(0)),
    asserta(prune_count(0)).

inc_node :-
    ( retract(node_count(N)) -> true ; N = 0 ),
    N1 is N + 1,
    asserta(node_count(N1)).

inc_prune :-
    ( retract(prune_count(P)) -> true ; P = 0 ),
    P1 is P + 1,
    asserta(prune_count(P1)).

get_nodes(N)  :- ( node_count(N)  -> true ; N = 0 ).
get_prunes(P) :- ( prune_count(P) -> true ; P = 0 ).

% =========================================================
% Board resolving
% - Si Board est une variable : on prend board/1 si dispo,
%   sinon un plateau vide.
% =========================================================

resolve_board(BoardIn, BoardOut) :-
    ( var(BoardIn) ->
        ( board(BoardOut) -> true
        ; empty_board(BoardOut)
        )
    ; BoardOut = BoardIn
    ).

empty_board([
    ['.','.','.','.','.','.','.'],
    ['.','.','.','.','.','.','.'],
    ['.','.','.','.','.','.','.'],
    ['.','.','.','.','.','.','.'],
    ['.','.','.','.','.','.','.'],
    ['.','.','.','.','.','.','.']
]).

% =========================================================
% Wrappers game.pl
% =========================================================

p_valid_moves(Board, Moves) :-
    findall(Col, (between(1,7,Col), valid_col(Board,Col)), Moves).

p_apply_move(Board, Player, Col, NewBoard) :-
    playMove(Board, Col, NewBoard, Player).

p_opponent(Player, Opp) :-
    changePlayer(Player, Opp).

p_is_draw(Board) :-
    board_full(Board).

% =========================================================
% Winner detection (IMPORTANT: interdit '.')
% =========================================================

p_winner(Board, P) :-
    ( P = 'x' ; P = 'o' ),
    ( win_h(Board, P)
    ; win_v(Board, P)
    ; win_d1(Board, P)
    ; win_d2(Board, P)
    ), !.

win_h(Board, P) :-
    member(Row, Board),
    append(_, [P,P,P,P|_], Row).

win_v(Board, P) :-
    between(1,7,C),
    column(Board, C, Col),
    append(_, [P,P,P,P|_], Col).

win_d1(Board, P) :-
    between(1,3,R),
    between(1,4,C),
    diag_down(Board, R, C, [P,P,P,P]).

win_d2(Board, P) :-
    between(4,6,R),
    between(1,4,C),
    diag_up(Board, R, C, [P,P,P,P]).

% =========================================================
% MINIMAX instrumenté (score du point de vue RootPlayer)
% =========================================================

profile_minimax(BoardIn, Player, Depth, profile(BestCol, Nodes)) :-
    reset_profile,
    resolve_board(BoardIn, Board),
    p_valid_moves(Board, Moves),
    ( Moves == [] ->
        BestCol = 0
    ;
        findall(Score-Col, (
            member(Col, Moves),
            p_apply_move(Board, Player, Col, B2),
            p_opponent(Player, Next),
            D1 is Depth - 1,
            p_minimax(B2, Next, D1, Player, Score)
        ), Scored),
        keysort(Scored, Sorted),
        last(Sorted, _BestScore-BestCol)
    ),
    get_nodes(Nodes).

p_minimax(Board, _ToPlay, _D, Root,  1000) :-
    inc_node,
    p_winner(Board, Root), !.

p_minimax(Board, _ToPlay, _D, Root, -1000) :-
    inc_node,
    p_winner(Board, W), W \= Root, !.

p_minimax(Board, _ToPlay, _D, _Root, 0) :-
    inc_node,
    p_is_draw(Board), !.

p_minimax(_Board, _ToPlay, 0, _Root, 0) :-
    inc_node, !.

p_minimax(Board, ToPlay, D, Root, Score) :-
    inc_node,
    D > 0,
    p_valid_moves(Board, Moves),
    ( Moves == [] ->
        Score = 0
    ;
        p_opponent(ToPlay, Next),
        D1 is D - 1,
        findall(V, (
            member(C, Moves),
            p_apply_move(Board, ToPlay, C, B2),
            p_minimax(B2, Next, D1, Root, V)
        ), Values),
        ( ToPlay == Root -> max_list_local(Values, Score)
        ;                 min_list_local(Values, Score)
        )
    ).

max_list_local([H|T], Max) :- max_list_local(T, H, Max).
max_list_local([], Max, Max).
max_list_local([H|T], Acc, Max) :-
    ( H > Acc -> Acc1 = H ; Acc1 = Acc ),
    max_list_local(T, Acc1, Max).

min_list_local([H|T], Min) :- min_list_local(T, H, Min).
min_list_local([], Min, Min).
min_list_local([H|T], Acc, Min) :-
    ( H < Acc -> Acc1 = H ; Acc1 = Acc ),
    min_list_local(T, Acc1, Min).

% =========================================================
% ALPHABETA instrumenté (Negamax + pruning count)
% =========================================================

profile_alphabeta(BoardIn, Player, Depth, ABBound, profile(BestCol, Nodes, Prunes)) :-
    reset_profile,
    resolve_board(BoardIn, Board),
    p_valid_moves(Board, Moves),
    ( Moves == [] ->
        BestCol = 0
    ;
        order_center_first(Moves, Ordered),
        p_ab_root(Ordered, Board, Player, Depth, -ABBound, ABBound, none, BestCol, _BestScore)
    ),
    get_nodes(Nodes),
    get_prunes(Prunes).

p_ab_root([], _Board, _Player, _D, _A, _B, Best, Best, -1000000).
p_ab_root([Col|Cols], Board, Player, D, A, B, BestSoFar, BestCol, BestScore) :-
    inc_node,
    p_apply_move(Board, Player, Col, B2),

    ( p_winner(B2, Player) ->
        Score is 100000
    ; p_winner(B2, W), W \= Player ->
        Score is -100000
    ; p_is_draw(B2) ->
        Score is 0
    ; D > 1 ->
        p_opponent(Player, Opp),
        D1 is D - 1,
        p_ab(B2, Opp, D1, -B, -A, ScoreNeg),
        Score is -ScoreNeg
    ;   Score is 0
    ),

    ( Score > A ->
        A1 = Score, Best1 = Col, BestScore1 = Score
    ;   A1 = A,     Best1 = BestSoFar, BestScore1 = BestScore
    ),

    ( A1 >= B ->
        inc_prune,
        BestCol = Best1, BestScore = BestScore1
    ;   p_ab_root(Cols, Board, Player, D, A1, B, Best1, BestCol, BestScore)
    ).

p_ab(Board, _Player, _D, _A, _B, 0) :-
    inc_node,
    p_is_draw(Board), !.
p_ab(_Board, _Player, 0, _A, _B, 0) :-
    inc_node, !.
p_ab(Board, Player, D, A, B, BestScore) :-
    inc_node,
    p_valid_moves(Board, Moves),
    ( Moves == [] ->
        BestScore = 0
    ; order_center_first(Moves, Ordered),
      p_opponent(Player, Opp),
      D1 is D - 1,
      p_ab_loop(Ordered, Board, Player, Opp, D1, A, B, -1000000, BestScore)
    ).

p_ab_loop([], _Board, _P, _Opp, _D, _A, _B, Best, Best).
p_ab_loop([Col|Cols], Board, Player, Opp, D1, A, B, BestSoFar, BestOut) :-
    inc_node,
    p_apply_move(Board, Player, Col, B2),

    ( p_winner(B2, Player) ->
        Score is 100000
    ; p_winner(B2, W), W \= Player ->
        Score is -100000
    ; p_is_draw(B2) ->
        Score is 0
    ;   p_ab(B2, Opp, D1, -B, -A, ScoreNeg),
        Score is -ScoreNeg
    ),

    Best1 is max(BestSoFar, Score),
    A1 is max(A, Score),

    ( A1 >= B ->
        inc_prune,
        BestOut = Best1
    ;   p_ab_loop(Cols, Board, Player, Opp, D1, A1, B, Best1, BestOut)
    ).

% =========================================================
% Move ordering : centre d abord
% =========================================================

order_center_first(Moves, Ordered) :-
    map_list_to_pairs(center_dist, Moves, Pairs),
    keysort(Pairs, Sorted),
    pairs_values(Sorted, Ordered).

center_dist(Col, Dist) :-
    Dist is abs(Col - 4).
