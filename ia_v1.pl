% ia_v1.pl
% IA v1 : choisit une colonne jouable au hasard.

:- use_module(library(random)).

% ia_v1(+Board, +Player, -Col)
% Player est là pour signature future (si tu veux faire une IA qui dépend du joueur),
% ici il n'est pas utilisé.
ia_v1(Board, _Player, Col) :-
    repeat,
    random_between(1, 7, Col),
    valid_col(Board, Col),
    !.
