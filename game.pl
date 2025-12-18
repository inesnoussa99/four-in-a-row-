% game.pl
% Logique du jeu Puissance 4 (plateau, coups, victoire), sans IA.

:- dynamic board/1.

% Remplace l ancien plateau par le nouveau
applyIt(Old, New) :-
    retract(board(Old)),
    assert(board(New)).

% Affichage du plateau
displayBoard :-
    board(B),
    nl,
    forall(nth1(_, B, Row),
        ( write('| '),
          forall(member(V, Row), (write(V), write(' '))),
          writeln('|')
        )),
    writeln(' -----------------'),
    writeln('  1 2 3 4 5 6 7'),
    nl.

% Une colonne est jouable si la case du HAUT (ligne 1) est encore '.'
valid_col(Board, Col) :-
    nth1(1, Board, TopRow),
    nth1(Col, TopRow, V),
    V == '.'. % jouable

% JOUER DANS UNE COLONNE
playMove(Board, Col, NewBoard, Player) :-
    find_free_row(Board, Col, Row),
    set_cell(Board, Row, Col, Player, NewBoard).

% Trouve la première ligne vide en partant du bas (Row=6 vers 1)
find_free_row(Board, Col, Row) :-
    find_free_row_from(6, Board, Col, Row).

find_free_row_from(0, _, _, _) :- !, fail.
find_free_row_from(R, Board, Col, Row) :-
    nth1(R, Board, Line),
    nth1(Col, Line, Cell),
    ( Cell = '.' ->
        Row = R
    ;   R1 is R-1,
        find_free_row_from(R1, Board, Col, Row)
    ).

% Met Player à (Row, Col)
set_cell(Board, Row, Col, Player, NewBoard) :-
    nth1(Row, Board, OldLine),
    replace(OldLine, Col, Player, NewLine),
    replace(Board, Row, NewLine, NewBoard).

% Remplacer un élément dans une liste
replace([_|T], 1, X, [X|T]).
replace([H|T], I, X, [H|R]) :-
    I > 1,
    I1 is I-1,
    replace(T, I1, X, R).

% Fin de partie : victoire si alignement
gameover(Board) :-
    ( win_h(Board)
    ; win_v(Board)
    ; win_d1(Board)
    ; win_d2(Board)
    ),
    !.

% Plateau plein (match nul)
board_full(Board) :-
    \+ (member(Row, Board), member('.', Row)).

% HORIZONTALES
win_h(Board) :-
    between(1, 6, Row),
    nth1(Row, Board, Line),
    adjacent4(Line).

% VERTICALES ↓
win_v(Board) :-
    between(1, 7, Col),
    column(Board, Col, ColList),
    adjacent4(ColList).

% Extraire une colonne
column([], _, []).
column([Row|Rest], Col, [V|R]) :-
    nth1(Col, Row, V),
    column(Rest, Col, R).

% DIAGONALES ↘
win_d1(Board) :-
    between(1, 3, Row),
    between(1, 4, Col),
    diag_down(Board, Row, Col, Diag),
    adjacent4(Diag).

diag_down(Board, Row, Col, [A,B,C,D]) :-
    nth1(Row,     Board, R1), nth1(Col,      R1, A),
    Row2 is Row+1, Col2 is Col+1,
    nth1(Row2,    Board, R2), nth1(Col2,     R2, B),
    Row3 is Row+2, Col3 is Col+2,
    nth1(Row3,    Board, R3), nth1(Col3,     R3, C),
    Row4 is Row+3, Col4 is Col+3,
    nth1(Row4,    Board, R4), nth1(Col4,     R4, D).

% DIAGONALES ↗
win_d2(Board) :-
    between(4, 6, Row),
    between(1, 4, Col),
    diag_up(Board, Row, Col, Diag),
    adjacent4(Diag).

diag_up(Board, Row, Col, [A,B,C,D]) :-
    nth1(Row,     Board, R1), nth1(Col,      R1, A),
    Row2 is Row-1, Col2 is Col+1,
    nth1(Row2,    Board, R2), nth1(Col2,     R2, B),
    Row3 is Row-2, Col3 is Col+2,
    nth1(Row3,    Board, R3), nth1(Col3,     R3, C),
    Row4 is Row-3, Col4 is Col+3,
    nth1(Row4,    Board, R4), nth1(Col4,     R4, D).

% Vérifier 4 identiques consécutifs dans une liste
adjacent4([A,B,C,D|_]) :-
    A \= '.',
    A == B, B == C, C == D,
    !.
adjacent4([_|T]) :-
    adjacent4(T).

% Changement de joueur
changePlayer('x','o').
changePlayer('o','x').

% Initialiser un plateau vide
init_board :-
    retractall(board(_)),
    Board = [
        ['.','.','.','.','.','.','.'],
        ['.','.','.','.','.','.','.'],
        ['.','.','.','.','.','.','.'],
        ['.','.','.','.','.','.','.'],
        ['.','.','.','.','.','.','.'],
        ['.','.','.','.','.','.','.']
    ],
    assert(board(Board)).
