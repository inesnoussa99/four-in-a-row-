% =========================
% File: profiling.pl
% Outils de comptage de nœuds explorés et pruning
% =========================

:- module(profiling, [
    reset_profile/0,
    profile_minimax/4,        % profile_minimax(+Board,+Player,+Depth,-profile(BestCol,Nodes))
    profile_alphabeta/5       % profile_alphabeta(+Board,+Player,+Depth,+AlphaBeta,-profile(BestCol,Nodes,Prunes))
]).

:- [game].
:- use_module(library(lists)).

:- dynamic node_count/1.
:- dynamic prune_count/1.

reset_profile :-
    retractall(node_count(_)),
    retractall(prune_count(_)),
    assertz(node_count(0)),
    assertz(prune_count(0)).

inc_node :-
    retract(node_count(N)), N1 is N + 1, assertz(node_count(N1)).
inc_prune :-
    retract(prune_count(P)), P1 is P + 1, assertz(prune_count(P1)).

get_nodes(N) :- node_count(N).
get_prunes(P) :- prune_count(P).

% =========================================================
% Wrappers game.pl
% =========================================================

p_valid_moves(Board, Moves) :-
    findall(Col, (between(1,7,Col), valid_col(Board,Col)), Moves).

p_apply_move(Board, Player, Col, NewBoard) :-
    playMove(Board, Col, NewBoard, Player).

p_opponent(Player, Opp) :-
    changePlayer(Player, Opp).

p_is_win(BoardAfterMove) :- gameover(BoardAfterMove).
p_is_draw(Board) :- board_full(Board).

% =========================================================
% MINIMAX instrumenté (générique pour x/o)
% =========================================================

profile_minimax(Board, Player, Depth, profile(BestCol, Nodes)) :-
    reset_profile,
    p_valid_moves(Board, Moves),
    ( Moves = [] -> BestCol = 0
    ; findall(Score-Col, (
            member(Col, Moves),
            p_apply_move(Board, Player, Col, B2),
            p_opponent(Player, Next),
            D1 is Depth - 1,
            p_minimax(B2, Next, D1, Player, Score)   % RootPlayer = Player
        ), Scored),
      keysort(Scored, Sorted),
      last(Sorted, _BestScore-BestCol)
    ),
    get_nodes(Nodes).

% p_minimax(+Board,+ToPlay,+Depth,+RootPlayer,-Score)
% Score vu du RootPlayer (MAX)
p_minimax(Board, _ToPlay, _D, Root, 1000) :-
    inc_node,
    p_is_win(Board),
    % si l'état est gagnant, le gagnant est le joueur qui vient de jouer,
    % mais on ne l’a pas => on approxime via la récursion : ici utilisé après un coup joué.
    % On garde une convention : victoire immédiate = très bon pour Root.
    !, Root = Root.
p_minimax(Board, _ToPlay, _D, _Root, 0) :-
    inc_node,
    p_is_draw(Board), !.
p_minimax(Board, _ToPlay, 0, _Root, 0) :-
    inc_node, !.
p_minimax(Board, ToPlay, D, Root, Score) :-
    inc_node,
    D > 0,
    p_valid_moves(Board, Moves),
    ( Moves = [] ->
        Score = 0
    ; p_opponent(ToPlay, Next),
      D1 is D - 1,
      findall(V, (
          member(C, Moves),
          p_apply_move(Board, ToPlay, C, B2),
          p_minimax(B2, Next, D1, Root, V)
      ), Values),
      ( ToPlay == Root -> max_list(Values, Score)
      ; min_list(Values, Score)
      )
    ).

% =========================================================
% ALPHABETA instrumenté (Negamax + pruning count)
% =========================================================

profile_alphabeta(Board, Player, Depth, AlphaBeta, profile(BestCol, Nodes, Prunes)) :-
    reset_profile,
    p_valid_moves(Board, Moves),
    order_center_first(Moves, Ordered),
    AB is AlphaBeta,
    p_ab_root(Ordered, Board, Player, Depth, -AB, AB, none, BestCol, _BestScore),
    get_nodes(Nodes),
    get_prunes(Prunes).

p_ab_root([], _Board, _Player, _D, _A, _B, Best, Best, -1000000).
p_ab_root([Col|Cols], Board, Player, D, A, B, BestSoFar, BestCol, BestScore) :-
    inc_node,
    p_apply_move(Board, Player, Col, B2),

    ( p_is_win(B2) ->
        Score is 100000
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
    ;   A1 = A, Best1 = BestSoFar, BestScore1 = BestScore
    ),

    ( A1 >= B ->
        inc_prune,
        BestCol = Best1,
        BestScore = BestScore1
    ;   p_ab_root(Cols, Board, Player, D, A1, B, Best1, BestCol, BestScore)
    ).

p_ab(Board, _Player, _D, _A, _B, 0) :-
    inc_node,
    p_is_draw(Board), !.
p_ab(Board, _Player, 0, _A, _B, 0) :-
    inc_node, !.
p_ab(Board, Player, D, A, B, BestScore) :-
    inc_node,
    p_valid_moves(Board, Moves),
    ( Moves = [] ->
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

    ( p_is_win(B2) ->
        Score is 100000
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

order_center_first(Moves, Ordered) :-
    map_list_to_pairs(center_dist, Moves, Pairs),
    keysort(Pairs, Sorted),
    pairs_values(Sorted, Ordered).

center_dist(Col, Dist) :- Dist is abs(Col - 4).
