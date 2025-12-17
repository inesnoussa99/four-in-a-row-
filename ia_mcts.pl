% =========================
% File: ia_mcts.pl
% =========================

:- [game].
:- use_module(library(lists)).
:- use_module(library(random)).

mcts_iterations(1200).
uct_c(1.4142).

% -------- WRAPPER game.pl
mcts_valid_moves(Board, Moves) :-
    findall(Col,
        ( between(1, 7, Col),
          valid_col(Board, Col)
        ),
        Moves).

mcts_apply_move(Board, Player, Col, NewBoard) :-
    playMove(Board, Col, NewBoard, Player).

mcts_opponent(Player, Opp) :-
    changePlayer(Player, Opp).

mcts_is_win(BoardAfterMove) :- gameover(BoardAfterMove).
mcts_is_draw(Board) :- board_full(Board).

% =========================================================
% API:
%   ia_mcts(+Board, +Player, -BestCol)
% =========================================================

ia_mcts(Board, Player, BestCol) :-
    mcts_valid_moves(Board, Moves),
    ( Moves = [Only] ->
        BestCol = Only
    ;   mcts_iterations(N),
        init_root(Moves, Root0),
        mcts_loop(N, Board, Player, Root0, RootF),
        best_child(RootF, BestCol)
    ).

% Node = node(Col, Visits, Wins)
init_root(Moves, root(0, 0, Children)) :-
    findall(node(C, 0, 0), member(C, Moves), Children).

mcts_loop(0, _Board, _Player, Root, Root).
mcts_loop(N, Board, Player, Root0, RootF) :-
    mcts_step(Board, Player, Root0, Root1),
    N1 is N - 1,
    mcts_loop(N1, Board, Player, Root1, RootF).

mcts_step(Board, Player, root(V0, W0, Children0), root(V1, W1, Children1)) :-
    select_uct(Children0, V0, BestNode, Rest),
    BestNode = node(Col, NV, NW),
    mcts_apply_move(Board, Player, Col, B2),

    ( mcts_is_win(B2) ->
        Result = 1.0
    ; mcts_is_draw(B2) ->
        Result = 0.5
    ;   mcts_opponent(Player, Opp),
        playout(B2, Opp, Player, Result)
    ),

    NV1 is NV + 1,
    NW1 is NW + Result,
    V1 is V0 + 1,
    W1 is W0 + Result,
    Children1 = [node(Col, NV1, NW1)|Rest].

% ----- UCT
select_uct([N|Ns], ParentVisits, Best, Rest) :-
    best_by_uct(Ns, ParentVisits, N, Best),
    select(Best, [N|Ns], Rest).

best_by_uct([], _PV, Best, Best).
best_by_uct([N|Ns], PV, CurBest, Best) :-
    ( uct_score(N, PV, S1),
      uct_score(CurBest, PV, S2),
      S1 > S2 -> Cur2 = N ; Cur2 = CurBest
    ),
    best_by_uct(Ns, PV, Cur2, Best).

uct_score(node(_Col, 0, _W), _PV, 1.0e9) :- !.
uct_score(node(_Col, V, W), PV, Score) :-
    uct_c(C),
    Exploit is W / V,
    Explore is C * sqrt(log(max(1, PV)) / V),
    Score is Exploit + Explore.

% ----- Playout random
% RootPlayer = joueur de référence (celui qui a lancé MCTS)
playout(Board, _ToPlay, RootPlayer, 1.0) :-
    mcts_is_win(Board), !,
    % IMPORTANT: mcts_is_win(Board) signifie "le joueur qui vient de jouer a gagné"
    % ici, RootPlayer n'est pas forcément celui qui vient de jouer,
    % mais comme on ne connait pas le gagnant explicitement, on approxime via la récursion:
    % on n'appelle playout/4 sur un état gagnant qu'après avoir joué un coup.
    true.

playout(Board, _ToPlay, _RootPlayer, 0.5) :-
    mcts_is_draw(Board), !.

playout(Board, ToPlay, RootPlayer, Result) :-
    mcts_valid_moves(Board, Moves),
    random_member(Col, Moves),
    mcts_apply_move(Board, ToPlay, Col, B2),
    ( mcts_is_win(B2) ->
        ( ToPlay == RootPlayer -> Result = 1.0 ; Result = 0.0 )
    ; mcts_is_draw(B2) ->
        Result = 0.5
    ;   mcts_opponent(ToPlay, Opp),
        playout(B2, Opp, RootPlayer, Result)
    ).

% ----- Choix final : le plus visité
best_child(root(_V, _W, Children), BestCol) :-
    sort_children_by_visits(Children, [node(BestCol, _, _)|_]).

sort_children_by_visits(Children, Sorted) :-
    map_list_to_pairs(visits_key, Children, Pairs),
    keysort(Pairs, SortedAsc),
    reverse(SortedAsc, SortedDesc),
    pairs_values(SortedDesc, Sorted).

visits_key(node(_C, V, _W), V).
