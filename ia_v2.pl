% ia_v2.pl
% IA v2 : si un coup gagnant existe, elle le joue.
% Sinon, elle utilise ia_v1 (aléatoire).

:- [ia_v1].   % On réutilise l'IA v1 comme fallback (random)
              % Assure-toi que ia_v1.pl est dans le même dossier.

% ia_v2(+Board, +Player, -Col)
% Choisit une colonne :
%  - d'abord un coup gagnant si possible,
%  - sinon un coup aléatoire jouable (ia_v1).

ia_v2(Board, Player, Col) :-
    (   winning_move(Board, Player, Col)
    ->  true
    ;   ia_v1(Board, Player, Col)
    ).

% winning_move(+Board, +Player, -Col)
% Vrai si Col est une colonne jouable telle que,
% en y jouant, Player gagne immédiatement.

winning_move(Board, Player, Col) :-
    between(1, 7, Col),           % tester les colonnes 1..7
    valid_col(Board, Col),        % la colonne est jouable
    playMove(Board, Col, NewBoard, Player),
    gameover(NewBoard),           % ce coup donne la victoire
    !.                            % on s'arrête au premier trouvé
