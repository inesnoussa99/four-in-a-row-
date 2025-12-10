% main.pl
% Point d'entrée du programme :
% - On choisit le type de joueur pour 'x' (Joueur 1) et pour 'o' (Joueur 2)
% - Puis on lance la partie avec ces deux "contrôleurs" (humain ou IA).

:- [game].
:- [ia_v1].
:- [ia_v2].
:- [ia_minimax].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%   Saisie des joueurs    %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% choix type :
%   human      -> humain
%   ia_v1      -> IA aléatoire
%   ia_v2      -> IA "winning move"
%   ia_minimax -> IA minimax

choose_player(PlayerSymbol, Type) :-
    format("Configure ~w :~n", [PlayerSymbol]),
    writeln('  1. Human'),
    writeln('  2. IA v1 (random)'),
    writeln('  3. IA v2 (winning move if possible)'),
    writeln('  4. IA minimax'),
    write('Your choice: '),
    read(Choice),
    map_choice_to_type(Choice, Type), !.

choose_player(PlayerSymbol, Type) :-
    writeln('Invalid choice, please try again.'),
    choose_player(PlayerSymbol, Type).

map_choice_to_type(1, human).
map_choice_to_type(2, ia_v1).
map_choice_to_type(3, ia_v2).
map_choice_to_type(4, ia_minimax).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%     Coup des joueurs    %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Demander un coup à un humain
ask_move_human(Player, Board, Col) :-
    format("Player ~w (human), choose a column (1-7): ", [Player]),
    read(Input),
    ( integer(Input),
      between(1, 7, Input),
      valid_col(Board, Input)
    ->
        Col = Input
    ;   writeln('Invalid column, please try again.'),
        ask_move_human(Player, Board, Col)
    ).

% Récupérer le type du joueur courant ('x' ou 'o')
get_player_type('x', TypeP1, _TypeP2, TypeP1).
get_player_type('o', _TypeP1, TypeP2, TypeP2).

% En fonction du type, obtenir le coup
get_move(human, Player, Board, Col) :-
    ask_move_human(Player, Board, Col).

get_move(ia_v1, Player, Board, Col) :-
    ia_v1(Board, Player, Col),
    format("IA v1 (~w) plays in column ~w~n", [Player, Col]).

get_move(ia_v2, Player, Board, Col) :-
    ia_v2(Board, Player, Col),
    format("IA v2 (~w) plays in column ~w~n", [Player, Col]).

get_move(ia_minimax, Player, Board, Col) :-
    ia_minimax(Board, Col),
    format("IA minimax (~w) plays in column ~w~n", [Player, Col]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%      Boucle de jeu      %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

play_loop(Player, TypeP1, TypeP2) :-
    board(Board),
    displayBoard,
    get_player_type(Player, TypeP1, TypeP2, Type),
    format("New turn for: ~w (~w)~n", [Player, Type]),
    get_move(Type, Player, Board, Col),
    ( playMove(Board, Col, NewBoard, Player) ->
        applyIt(Board, NewBoard),
        ( gameover(NewBoard) ->
            displayBoard,
            format("Player ~w wins!~n", [Player]),
            retractall(board(_))
        ; board_full(NewBoard) ->
            displayBoard,
            writeln('Draw!'),
            retractall(board(_))
        ;
            changePlayer(Player, NextPlayer),
            play_loop(NextPlayer, TypeP1, TypeP2)
        )
    ;
        writeln('Move failed, retrying.'),
        play_loop(Player, TypeP1, TypeP2)
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%          MAIN           %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

main :-
    writeln('======================'),
    writeln('       PUISSANCE 4    '),
    writeln('======================'),
    writeln('Configure players:'),
    choose_player('Player 1 = x', TypeP1),
    choose_player('Player 2 = o', TypeP2),
    init_board,
    play_loop('x', TypeP1, TypeP2).
