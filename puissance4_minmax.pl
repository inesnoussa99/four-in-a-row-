:- dynamic board/1.

applyIt(Old,New) :- retract(board(Old)), assert(board(New)).

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

depth(4).

ia_minmax(Board, BestCol) :-
    depth(D),
    findall(Col, valid_col(Board, Col), Moves),
    evaluate_moves(Board, Moves, D, ScoredMoves),
    keysort(ScoredMoves, SortedMoves),
    last(SortedMoves, Score-BestCol),
    write('IA estime ce coup a : '), writeln(Score).

evaluate_moves(_, [], _, []).
evaluate_moves(Board, [Col|Rest], Depth, [Score-Col|ScoreRest]) :-
    playMove(Board, Col, NewBoard, 'o'),
    minimax(NewBoard, Depth, 'x', Score),
    evaluate_moves(Board, Rest, Depth, ScoreRest).

minimax(Board, _, _, 1000) :- win_player(Board, 'o'), !.
minimax(Board, _, _, -1000) :- win_player(Board, 'x'), !.
minimax(Board, _, _, 0) :- \+ (member(Row, Board), member('.', Row)), !.
minimax(Board, 0, _, Score) :- heuristic(Board, Score), !.
minimax(Board, Depth, Player, Score) :-
    Depth > 0,
    D1 is Depth - 1,
    findall(C, valid_col(Board, C), Moves),
    ( Moves = [] -> Score = 0 ; % Cas rare de match nul inattendu
        changePlayer(Player, NextPlayer),
        findall(Val, (
            member(M, Moves),
            playMove(Board, M, NewB, Player),
            minimax(NewB, D1, NextPlayer, Val)
        ), Values),
        (Player == 'o' -> max_list(Values, Score) ; min_list(Values, Score))
    ).

heuristic(Board, Score) :-
    findall(S, score_lines(Board, S), Scores),
    sum_list(Scores, Score).

score_lines(Board, Score) :-
    (get_line_h(Board, Line) ; get_line_v(Board, Line) ; get_line_d(Board, Line)),
    score_4_cells(Line, Score).

score_4_cells(List, Score) :-
    subtract(List, ['.'], Filled), % Enlève les points vides
    length(Filled, Len),
    ( Len == 0 -> Score = 0
    ; exclude(==('x'), Filled, OnlyO), length(OnlyO, LenO),
      exclude(==('o'), Filled, OnlyX), length(OnlyX, LenX),
      ( LenO == 3, LenX == 0 -> Score = 50      % 3 pions IA + 1 vide
      ; LenO == 2, LenX == 0 -> Score = 10      % 2 pions IA + 2 vides
      ; LenX == 3, LenO == 0 -> Score = -50     % 3 pions Humain (Danger !)
      ; LenX == 2, LenO == 0 -> Score = -10     % 2 pions Humain
      ; Score = 0
      )
    ).

get_line_h(Board, [A,B,C,D]) :-
    member(Row, Board),
    append(_, [A,B,C,D|_], Row).

get_line_v(Board, [A,B,C,D]) :-
    column(Board, _, ColList), 
    append(_, [A,B,C,D|_], ColList).

get_line_d(Board, Diag) :-
    (win_d1_gen(Board, Diag) ; win_d2_gen(Board, Diag)).

% Helpers pour les diagonales (basés sur ta logique win_d1/d2 mais qui renvoient la liste)
win_d1_gen(Board, [A,B,C,D]) :-
    between(1,3,Row), 
    between(1,4,Col), 
    diag_down(Board, Row, Col, [A,B,C,D]).

win_d2_gen(Board, [A,B,C,D]) :-
    between(4,6,Row), 
    between(1,4,Col), 
    diag_up(Board, Row, Col, [A,B,C,D]).

win_player(Board, Player) :-
    (win_h_p(Board, Player) ; win_v_p(Board, Player) ; win_d1_p(Board, Player) ; win_d2_p(Board, Player)).

win_h_p(Board, P) :- member(Row, Board), append(_, [P,P,P,P|_], Row).
win_v_p(Board, P) :- between(1,7,C), column(Board, C, Col), append(_, [P,P,P,P|_], Col).
win_d1_p(Board, P) :- win_d1_gen(Board, [P,P,P,P]).
win_d2_p(Board, P) :- win_d2_gen(Board, [P,P,P,P]).

sum_list([], 0).
sum_list([H|T], S) :- sum_list(T, S1), S is H + S1.
max_list([H|T], Max) :- max_list(T, H, Max).
max_list([], Max, Max).
max_list([H|T], Acc, Max) :- H > Acc, !, max_list(T, H, Max).
max_list([_|T], Acc, Max) :- max_list(T, Acc, Max).
min_list([H|T], Min) :- min_list(T, H, Min).
min_list([], Min, Min).
min_list([H|T], Acc, Min) :- H < Acc, !, min_list(T, H, Min).
min_list([_|T], Acc, Min) :- min_list(T, Acc, Min).

valid_col(Board, Col) :-
    nth1(1, Board, TopRow),
    nth1(Col, TopRow, V),
    V == '.'.

playMove(Board, Col, NewBoard, Player) :-
    find_free_row(Board, Col, Row),
    set_cell(Board, Row, Col, Player, NewBoard).

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

set_cell(Board, Row, Col, Player, NewBoard) :-
    nth1(Row, Board, OldLine),
    replace(OldLine, Col, Player, NewLine),
    replace(Board, Row, NewLine, NewBoard).

replace([_|T], 1, X, [X|T]).
replace([H|T], I, X, [H|R]) :- I > 1, I1 is I-1, replace(T, I1, X, R).

gameover(Board, _) :-
    ( win_h(Board)
    ; win_v(Board)
    ; win_d1(Board)
    ; win_d2(Board)
    ), !.

win_h(Board) :-
    between(1,6,Row),
    nth1(Row, Board, Line),
    adjacent4(Line).

win_v(Board) :-
    between(1,7,Col),
    column(Board, Col, ColList),
    adjacent4(ColList).

column([], _, []).
column([Row|Rest], Col, [V|R]) :-
    nth1(Col, Row, V),
    column(Rest, Col, R).

win_d1(Board) :-
    between(1,3,Row),
    between(1,4,Col),
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

win_d2(Board) :-
    between(4,6,Row),
    between(1,4,Col),
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

adjacent4([A,B,C,D|_]) :-
    A \= '.',
    A == B, B == C, C == D, !.
adjacent4([_|T]) :- adjacent4(T).

changePlayer('x','o').
changePlayer('o','x').

human_turn(Board, Col) :-
    write('Your move (1-7): '),
    read(UserInput),
    integer(UserInput),
    between(1,7,UserInput),
    valid_col(Board, UserInput),
    Col = UserInput, !.

human_turn(Board, Col) :-
    writeln('Invalid move. Try again.'),
    human_turn(Board, Col).

play(Player) :-
    write('New turn for: '), writeln(Player),
    board(Board),
    displayBoard,
    ( Player == 'x' ->
        human_turn(Board, Move)
    ; ia_minmax(Board, Move)
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
