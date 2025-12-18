% Importation de bibliothèques utiles
:- use_module(library(lists)).   % Pour manipuler des listes
:- use_module(library(apply)).   % Pour forall/2
:- dynamic board/1.              % Le plateau est dynamique (modifiable)

% Remplace l ancien plateau par le nouveau
applyIt(Old,New) :-
    retract(board(Old)),
    assert(board(New)).

% Affiche le plateau de jeu
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

% Vérifie si une colonne est valide (si la case du haut est vide)
valid_col(Board, Col) :-
    nth1(1, Board, TopRow),
    nth1(Col, TopRow, V),
    V == '.'.

% Joue un coup dans une colonne pour un joueur
playMove(Board, Col, NewBoard, Player) :-
    find_free_row(Board, Col, Row),     % Trouve la ligne libre
    set_cell(Board, Row, Col, Player, NewBoard).

% Cherche la première ligne libre en partant du bas
find_free_row(Board, Col, Row) :-
    find_free_row_from(6, Board, Col, Row).

% Cas d échec : plus de ligne à tester
find_free_row_from(0, _, _, _) :- !, fail.

% Si la case est vide, on prend cette ligne
find_free_row_from(R, Board, Col, Row) :-
    nth1(R, Board, Line),
    nth1(Col, Line, Cell),
    ( Cell = '.' ->
        Row = R
    ;   R1 is R-1,
        find_free_row_from(R1, Board, Col, Row)
    ).

% Place le pion du joueur dans la bonne case
set_cell(Board, Row, Col, Player, NewBoard) :-
    nth1(Row, Board, OldLine),
    replace(OldLine, Col, Player, NewLine),
    replace(Board, Row, NewLine, NewBoard).

% Remplace un élément dans une liste
replace([_|T], 1, X, [X|T]).
replace([H|T], I, X, [H|R]) :-
    I > 1,
    I1 is I-1,
    replace(T, I1, X, R).

% Vérifie si la partie est terminée (victoire)
gameover(Board, _) :-
    ( win_h(Board)
    ; win_v(Board)
    ; win_d1(Board)
    ; win_d2(Board)
    ), !.

% Victoire horizontale
win_h(Board) :-
    between(1,6,Row),
    nth1(Row, Board, Line),
    adjacent4(Line).

% Victoire verticale
win_v(Board) :-
    between(1,7,Col),
    column(Board, Col, ColList),
    adjacent4(ColList).

% Récupère une colonne du plateau
column([], _, []).
column([Row|Rest], Col, [V|R]) :-
    nth1(Col, Row, V),
    column(Rest, Col, R).

% Victoire diagonale descendante (\)
win_d1(Board) :-
    between(1,3,Row),
    between(1,4,Col),
    diag_down(Board, Row, Col, Diag),
    adjacent4(Diag).

% Récupère une diagonale descendante de 4 cases
diag_down(Board, Row, Col, [A,B,C,D]) :-
    nth1(Row, Board, R1), nth1(Col, R1, A),
    Row2 is Row+1, Col2 is Col+1,
    nth1(Row2, Board, R2), nth1(Col2, R2, B),
    Row3 is Row+2, Col3 is Col+2,
    nth1(Row3, Board, R3), nth1(Col3, R3, C),
    Row4 is Row+3, Col4 is Col+3,
    nth1(Row4, Board, R4), nth1(Col4, R4, D).

% Victoire diagonale montante 
win_d2(Board) :-
    between(4,6,Row),
    between(1,4,Col),
    diag_up(Board, Row, Col, Diag),
    adjacent4(Diag).

% Récupère une diagonale montante de 4 cases
diag_up(Board, Row, Col, [A,B,C,D]) :-
    nth1(Row, Board, R1), nth1(Col, R1, A),
    Row2 is Row-1, Col2 is Col+1,
    nth1(Row2, Board, R2), nth1(Col2, R2, B),
    Row3 is Row-2, Col3 is Col+2,
    nth1(Row3, Board, R3), nth1(Col3, R3, C),
    Row4 is Row-3, Col4 is Col+3,
    nth1(Row4, Board, R4), nth1(Col4, R4, D).

% Vérifie sil y a 4 pions identiques consécutifs
adjacent4([A,B,C,D|_]) :-
    A \= '.',
    A == B, B == C, C == D, !.
adjacent4([_|T]) :-
    adjacent4(T).

% Change le joueur courant
changePlayer('x','o').
changePlayer('o','x').

% Tour du joueur humain
human_turn(Board, Col) :-
    write('Your move (1-7): '),
    read_line_to_codes(user_input, Codes),
    string_codes(Str, Codes),
    normalize_space(string(Clean), Str),
    catch(number_string(UserInput, Clean), _, fail),
    integer(UserInput),
    between(1,7,UserInput),
    valid_col(Board, UserInput),
    Col = UserInput, !.

% En cas de coup invalide
human_turn(Board, Col) :-
    writeln('Invalid move. Try again.'),
    human_turn(Board, Col).

% Coup gagnant pour lIA
winning_move(Board, Player, Col) :-
    between(1,7,Col),
    valid_col(Board, Col),
    playMove(Board, Col, NewBoard, Player),
    gameover(NewBoard, Player).

% Coup pour bloquer ladversaire
blocking_move(Board, Opponent, Col) :-
    between(1,7,Col),
    valid_col(Board, Col),
    playMove(Board, Col, NewBoard, Opponent),
    gameover(NewBoard, Opponent).

% Ordre de préférence des colonnes 
preferred_move(_, Col) :-
    member(Col, [4, 3, 5, 2, 6, 1, 7]).

% Intelligence artificielle
ia(Board, Col) :-
    winning_move(Board, 'o', Col), !.
ia(Board, Col) :-
    blocking_move(Board, 'x', Col), !.
ia(Board, Col) :-
    preferred_move(Board,Col),
    valid_col(Board, Col), !.

% Boucle principale du jeu
play(Player) :-
    write('New turn for: '), writeln(Player),
    board(Board),
    displayBoard,
    ( Player == 'x' ->
        human_turn(Board, Move)
    ; ia(Board, Move)
    ),
    ( playMove(Board, Move, NewBoard, Player) ->
        ( Player == 'x' -> write('You play in column ') ; write('IA plays in column ') ),
        writeln(Move),
        applyIt(Board, NewBoard),
        ( gameover(NewBoard, Move) ->
            displayBoard,
            (Player == 'x' -> writeln('You win!') ; writeln('IA wins!')),
            retractall(board(_))
        ; \+ (member(Row, NewBoard), member('.', Row)) ->
            displayBoard, writeln('Draw!'), retractall(board(_))
        ;
            changePlayer(Player, NextPlayer),
            play(NextPlayer)
        )
    ;
        play(Player)
    ).

% Initialisation du jeu
init :-
    retractall(board(_)),
    Board = [
        ['.','.','.','.','.','.','.'],
        ['.','.','.','.','.','.','.'],
        ['.','.','.','.','.','.','.'],
        ['.','.','.','.','.','.','.'],
        ['.','.','.','.','.','.','.'],
        ['.','.','.','.','.','.','.']
    ],
    assert(board(Board)),
    play('x').
