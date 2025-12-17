% ia_minimax.pl
% IA minimax pour Puissance 4.
% Hypothèses :
%   - Le jeu est déjà défini dans game.pl avec :
%       valid_col/2, playMove/4, changePlayer/2,
%       column/3, diag_down/4, diag_up/4, etc.
%   - 'o' = IA (max), 'x' = humain (min).

% Profondeur de recherche du minimax
depth(1).

% ia_minimax(+Board, -BestCol)
% Choisit la meilleure colonne selon minimax (vue du joueur 'o').
ia_minimax(Board, BestCol) :-
    depth(D),
    findall(Col, valid_col(Board, Col), Moves),
    evaluate_moves(Board, Moves, D, ScoredMoves),
    keysort(ScoredMoves, SortedMoves),
    last(SortedMoves, Score-BestCol),
    write('IA minimax estime ce coup à : '), writeln(Score).

% evaluate_moves(+Board, +Moves, +Depth, -ScoredMoves)
% Associe à chaque coup Col un score Score-Col.
evaluate_moves(_, [], _, []).
evaluate_moves(Board, [Col|Rest], Depth, [Score-Col|ScoreRest]) :-
    % On suppose que l’IA est 'o' et joue ce coup
    playMove(Board, Col, NewBoard, 'o'),
    % Ensuite ce sera à 'x' de jouer
    minimax(NewBoard, Depth, 'x', Score),
    evaluate_moves(Board, Rest, Depth, ScoreRest).

%%%%%%%%%%%%%%%%%%%%
%     MINIMAX      %
%%%%%%%%%%%%%%%%%%%%

% Victoire IA -> gros score positif
minimax(Board, _, _, 1000) :-
    win_player(Board, 'o'), !.

% Victoire humain -> gros score négatif
minimax(Board, _, _, -1000) :-
    win_player(Board, 'x'), !.

% Plateau plein -> match nul
minimax(Board, _, _, 0) :-
    \+ (member(Row, Board), member('.', Row)), !.

% Profondeur 0 -> évaluation heuristique
minimax(Board, 0, _, Score) :-
    heuristic(Board, Score), !.

% Cas général
minimax(Board, Depth, Player, Score) :-
    Depth > 0,
    D1 is Depth - 1,
    findall(C, valid_col(Board, C), Moves),
    (   Moves = [] ->
        Score = 0                  % aucun coup possible
    ;   changePlayer(Player, NextPlayer),
        findall(Val, (
            member(M, Moves),
            playMove(Board, M, NewB, Player),
            minimax(NewB, D1, NextPlayer, Val)
        ), Values),
        (   Player == 'o'
        ->  max_list(Values, Score)  % IA maximise
        ;   min_list(Values, Score)  % humain minimise
        )
    ).

%%%%%%%%%%%%%%%%%%%%
%   HEURISTIQUE    %
%%%%%%%%%%%%%%%%%%%%

% heuristic(+Board, -Score)
% Additionne les scores de toutes les "lignes" possibles de 4 cases.
heuristic(Board, Score) :-
    findall(S, score_lines(Board, S), Scores),
    sum_list(Scores, Score).

% score_lines(+Board, -Score)
% Génère toutes les lignes de 4 cases (horiz, vert, diag),
% et calcule un Score pour chacune.
score_lines(Board, Score) :-
    (   get_line_h(Board, Line)
    ;   get_line_v(Board, Line)
    ;   get_line_d(Board, Line)
    ),
    score_4_cells(Line, Score).

% score_4_cells(+List4, -Score)
% Évalue une fenêtre de 4 cases du point de vue de l’IA 'o'.
score_4_cells(List, Score) :-
    subtract(List, ['.'], Filled), % enlève les cases vides
    length(Filled, Len),
    (   Len == 0 ->
        Score = 0
    ;   exclude(==('x'), Filled, OnlyO), length(OnlyO, LenO),
        exclude(==('o'), Filled, OnlyX), length(OnlyX, LenX),
        (   LenO == 3, LenX == 0 -> Score = 50      % 3 pions IA + 1 vide
        ;   LenO == 2, LenX == 0 -> Score = 10      % 2 pions IA + 2 vides
        ;   LenX == 3, LenO == 0 -> Score = -50     % 3 pions humain (danger)
        ;   LenX == 2, LenO == 0 -> Score = -10     % 2 pions humain
        ;   Score = 0
        )
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   LIGNES DE 4 POSSIBLES  %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Lignes horizontales
get_line_h(Board, [A,B,C,D]) :-
    member(Row, Board),
    append(_, [A,B,C,D|_], Row).

% Lignes verticales
get_line_v(Board, [A,B,C,D]) :-
    column(Board, _, ColList),
    append(_, [A,B,C,D|_], ColList).

% Diagonales
get_line_d(Board, Diag) :-
    (   win_d1_gen(Board, Diag)
    ;   win_d2_gen(Board, Diag)
    ).

% Helpers pour les diagonales (génèrent toutes les diag de 4)
win_d1_gen(Board, [A,B,C,D]) :-
    between(1,3,Row),
    between(1,4,Col),
    diag_down(Board, Row, Col, [A,B,C,D]).

win_d2_gen(Board, [A,B,C,D]) :-
    between(4,6,Row),
    between(1,4,Col),
    diag_up(Board, Row, Col, [A,B,C,D]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   DÉTECTION DE VICTOIRE  %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% win_player(+Board, +Player)
% Vrai si Player a 4 pions alignés.
win_player(Board, Player) :-
    (   win_h_p(Board, Player)
    ;   win_v_p(Board, Player)
    ;   win_d1_p(Board, Player)
    ;   win_d2_p(Board, Player)
    ).

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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   PETITES FONCTIONS      %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sum_list([], 0).
sum_list([H|T], S) :-
    sum_list(T, S1),
    S is H + S1.

max_list([H|T], Max) :-
    max_list(T, H, Max).
max_list([], Max, Max).
max_list([H|T], Acc, Max) :-
    H > Acc, !,
    max_list(T, H, Max).
max_list([_|T], Acc, Max) :-
    max_list(T, Acc, Max).

min_list([H|T], Min) :-
    min_list(T, H, Min).
min_list([], Min, Min).
min_list([H|T], Acc, Min) :-
    H < Acc, !,
    min_list(T, H, Min).
min_list([_|T], Acc, Min) :-
    min_list(T, Acc, Min).
