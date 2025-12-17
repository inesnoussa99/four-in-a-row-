% =========================
% File: simulator.pl
% Simulation IA vs IA (sans module, robuste, PAS de win('.'))
% =========================

:- [game].
:- [ia_v1].
:- [ia_v2].
:- [ia_minimax].
:- [ia_alphabeta].

:- use_module(library(random)).
:- use_module(library(apply)).   % maplist/3

% -------------------------
% API :
%   simulate_game(+TypeX,+TypeO,+StartPlayer,-Result)
%   simulate_games(+TypeX,+TypeO,+N,+AltStart,-Summary)
%   print_summary(+Summary)
%
% Result  = win('x') | win('o') | draw
% Summary = summary(WX, WO, D)
% -------------------------

simulate_games(TypeX, TypeO, N, AltStart, summary(WX, WO, D)) :-
    simulate_games_(1, N, TypeX, TypeO, AltStart, 0, 0, 0, WX, WO, D), !.

simulate_games_(I, N, _TX, _TO, _Alt, WX, WO, D, WX, WO, D) :-
    I > N, !.
simulate_games_(I, N, TX, TO, Alt, WX0, WO0, D0, WX, WO, D) :-
    ( Alt == true ->
        ( 0 is I mod 2 -> Start = 'o' ; Start = 'x' )
    ;   Start = 'x'
    ),
    simulate_game(TX, TO, Start, R),
    update_counts(R, WX0, WO0, D0, WX1, WO1, D1),
    I1 is I + 1,
    simulate_games_(I1, N, TX, TO, Alt, WX1, WO1, D1, WX, WO, D).

simulate_game(TypeX, TypeO, StartPlayer, Result) :-
    empty_board(Board0),
    ( catch(simulate_loop(Board0, StartPlayer, TypeX, TypeO, 0, Result0), _, Result0 = draw) ->
        Result = Result0
    ;   Result = draw
    ),
    !.

print_summary(summary(WX, WO, D)) :-
    Total is WX + WO + D,
    format("=== Simulation summary ===~n", []),
    format("Games : ~w~n", [Total]),
    format("X wins: ~w~n", [WX]),
    format("O wins: ~w~n", [WO]),
    format("Draws : ~w~n", [D]).

update_counts(win('x'), WX0, WO0, D0, WX1, WO0, D0) :- WX1 is WX0 + 1.
update_counts(win('o'), WX0, WO0, D0, WX0, WO1, D0) :- WO1 is WO0 + 1.
update_counts(draw,     WX0, WO0, D0, WX0, WO0, D1) :- D1 is D0 + 1.
% sécurité (au cas où)
update_counts(_, WX, WO, D, WX, WO, D).

% =========================
% Boucle de jeu
% =========================

simulate_loop(Board, _Player, _TX, _TO, _Moves, win(W)) :-
    winner_player(Board, W), !.
simulate_loop(Board, _Player, _TX, _TO, _Moves, draw) :-
    board_full(Board), !.
simulate_loop(_Board, _Player, _TX, _TO, Moves, draw) :-
    Moves >= 42, !.

simulate_loop(Board, Player, TypeX, TypeO, Moves0, Result) :-
    sim_valid_moves(Board, Moves),
    ( Moves == [] ->
        Result = draw
    ;
        choose_type(Player, TypeX, TypeO, Type),
        safe_choose_move(Type, Player, Board, Moves, Col),
        safe_play_move(Board, Moves, Col, Player, Board2),
        changePlayer(Player, Next),
        Moves1 is Moves0 + 1,
        simulate_loop(Board2, Next, TypeX, TypeO, Moves1, Result)
    ).

choose_type('x', TypeX, _TypeO, TypeX).
choose_type('o', _TypeX, TypeO, TypeO).

sim_valid_moves(Board, Moves) :-
    findall(Col, (between(1,7,Col), valid_col(Board, Col)), Moves).

safe_choose_move(Type, Player, Board, Moves, ColOut) :-
    ( catch(choose_move(Type, Player, Board, Col), _, fail),
      integer(Col),
      member(Col, Moves)
    ->
      ColOut = Col
    ;
      random_member(ColOut, Moves)
    ).

safe_play_move(Board, Moves, Col, Player, Board2) :-
    ( playMove(Board, Col, Board2, Player) ->
        true
    ;   random_member(Col2, Moves),
        playMove(Board, Col2, Board2, Player)
    ).

% =========================
% Choix du coup (interfaces)
% =========================

choose_move(ia_v1, Player, Board, Col) :- ia_v1(Board, Player, Col).
choose_move(ia_v2, Player, Board, Col) :- ia_v2(Board, Player, Col).
choose_move(ia_alphabeta, Player, Board, Col) :- ia_alphabeta(Board, Player, Col).

% ia_minimax/2 est "vue du joueur o"
choose_move(ia_minimax, 'o', Board, Col) :- ia_minimax(Board, Col).
choose_move(ia_minimax, 'x', Board, Col) :-
    swap_board(Board, Swapped),
    ia_minimax(Swapped, Col).

% =========================
% Utilitaires plateau
% =========================

empty_board([
    ['.','.','.','.','.','.','.'],
    ['.','.','.','.','.','.','.'],
    ['.','.','.','.','.','.','.'],
    ['.','.','.','.','.','.','.'],
    ['.','.','.','.','.','.','.'],
    ['.','.','.','.','.','.','.']
]).

swap_board(Board, Swapped) :- maplist(swap_row, Board, Swapped).
swap_row([], []).
swap_row([H|T], [H2|T2]) :- swap_cell(H,H2), swap_row(T,T2).
swap_cell('x','o') :- !.
swap_cell('o','x') :- !.
swap_cell('.','.').

% =========================
% Gagnant (IMPORTANT : interdit P='.')
% =========================

winner_player(Board, P) :-
    ( P = 'x' ; P = 'o' ),              % ✅ pas '.'
    ( win_h(Board, P)
    ; win_v(Board, P)
    ; win_d1(Board, P)
    ; win_d2(Board, P)
    ), !.

win_h(Board, P) :- member(Row, Board), append(_, [P,P,P,P|_], Row).
win_v(Board, P) :- between(1,7,C), column(Board, C, Col), append(_, [P,P,P,P|_], Col).
win_d1(Board, P) :- between(1,3,R), between(1,4,C), diag_down(Board, R, C, [P,P,P,P]).
win_d2(Board, P) :- between(4,6,R), between(1,4,C), diag_up(Board, R, C, [P,P,P,P]).
