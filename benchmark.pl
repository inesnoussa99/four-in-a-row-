% =========================
% File: benchmark.pl
% Outils de mesure de performance (temps, inférences)
% =========================

:- module(benchmark, [
    benchmark_games/6,     % benchmark_games(+TypeX,+TypeO,+N,+AltStart,+MaxMoves,-Report)
    print_report/1
]).

:- [game].
:- [ia_v1].
:- [ia_v2].
:- [ia_minimax].
:- [ia_alphabeta].

:- use_module(library(random)).
:- use_module(library(apply)). % maplist/3

% Report = report(Games, Moves, WX, WO, D, TimeSec, Inferences, Aborted)
benchmark_games(TypeX, TypeO, N, AltStart, MaxMoves, Report) :-
    statistics(inferences, I0),
    get_time(T0),

    bench_loop(1, N, TypeX, TypeO, AltStart, MaxMoves,
               0, 0, 0, 0, 0,        % winsX,winsO,draws,totalMoves,aborted
               WX, WO, D, Moves, Aborted),

    get_time(T1),
    statistics(inferences, I1),

    Time is T1 - T0,
    Inf  is I1 - I0,
    Report = report(N, Moves, WX, WO, D, Time, Inf, Aborted).

print_report(report(G, M, WX, WO, D, Time, Inf, Ab)) :-
    format("=== Benchmark ===~n", []),
    format("Games        : ~w~n", [G]),
    format("Total moves  : ~w~n", [M]),
    format("X wins       : ~w~n", [WX]),
    format("O wins       : ~w~n", [WO]),
    format("Draws        : ~w~n", [D]),
    format("Aborted      : ~w~n", [Ab]),
    format("Time (s)     : ~6f~n", [Time]),
    format("Inferences   : ~w~n", [Inf]),
    ( M > 0 ->
        SecPerMove is Time / M,
        InfPerMove is Inf / M,
        format("Avg sec/move: ~6f~n", [SecPerMove]),
        format("Avg inf/move: ~2f~n", [InfPerMove])
    ; true
    ).

% =========================
% Boucle benchmark
% =========================

bench_loop(I, N, _TX, _TO, _Alt, _MaxMoves,
           WX, WO, D, Moves, Aborted,
           WX, WO, D, Moves, Aborted) :-
    I > N, !.

bench_loop(I, N, TX, TO, Alt, MaxMoves,
           WX0, WO0, D0, Moves0, Ab0,
           WX, WO, D, Moves, Ab) :-

    ( Alt == true ->
        ( 0 is I mod 2 -> Start = 'o' ; Start = 'x' )
    ;   Start = 'x'
    ),

    empty_board(Board0),

    % sécurité : si une partie plante, on la considère abort
    ( catch(bench_game(Board0, Start, TX, TO, 0, MaxMoves, Result, MovesInGame), _, fail) ->
        true
    ;   Result = abort,
        MovesInGame = 0
    ),

    Moves1 is Moves0 + MovesInGame,
    update_counts(Result, WX0, WO0, D0, Ab0, WX1, WO1, D1, Ab1),

    I1 is I + 1,
    bench_loop(I1, N, TX, TO, Alt, MaxMoves, WX1, WO1, D1, Moves1, Ab1, WX, WO, D, Moves, Ab).

update_counts(win('x'), WX0, WO0, D0, Ab0, WX1, WO0, D0, Ab0) :- WX1 is WX0 + 1.
update_counts(win('o'), WX0, WO0, D0, Ab0, WX0, WO1, D0, Ab0) :- WO1 is WO0 + 1.
update_counts(draw,     WX0, WO0, D0, Ab0, WX0, WO0, D1, Ab0) :- D1 is D0 + 1.
update_counts(abort,    WX0, WO0, D0, Ab0, WX0, WO0, D0, Ab1) :- Ab1 is Ab0 + 1.
update_counts(_,        WX,  WO,  D,  Ab,  WX, WO, D, Ab). % sécurité

% =========================
% Partie (robuste)
% =========================

bench_game(Board, _Player, _TX, _TO, Moves, _MaxMoves, win(W), Moves) :-
    winner_player(Board, W), !.
bench_game(Board, _Player, _TX, _TO, Moves, _MaxMoves, draw, Moves) :-
    board_full(Board), !.
bench_game(_Board, _Player, _TX, _TO, Moves, MaxMoves, abort, Moves) :-
    Moves >= MaxMoves, !.

bench_game(Board, Player, TX, TO, Moves0, MaxMoves, Result, MovesOut) :-
    sim_valid_moves(Board, ValidMoves),
    ( ValidMoves == [] ->
        Result = draw,
        MovesOut = Moves0
    ;
        choose_type(Player, TX, TO, Type),
        safe_choose_move(Type, Player, Board, ValidMoves, Col),
        safe_play_move(Board, ValidMoves, Col, Player, Board2),
        changePlayer(Player, Next),
        Moves1 is Moves0 + 1,
        bench_game(Board2, Next, TX, TO, Moves1, MaxMoves, Result, MovesOut)
    ).

choose_type('x', TX, _TO, TX).
choose_type('o', _TX, TO, TO).

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
% Plateau
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
    ( P = 'x' ; P = 'o' ),
    ( win_h(Board, P)
    ; win_v(Board, P)
    ; win_d1(Board, P)
    ; win_d2(Board, P)
    ), !.

win_h(Board, P) :- member(Row, Board), append(_, [P,P,P,P|_], Row).
win_v(Board, P) :- between(1,7,C), column(Board, C, Col), append(_, [P,P,P,P|_], Col).
win_d1(Board, P) :- between(1,3,R), between(1,4,C), diag_down(Board, R, C, [P,P,P,P]).
win_d2(Board, P) :- between(4,6,R), between(1,4,C), diag_up(Board, R, C, [P,P,P,P]).
