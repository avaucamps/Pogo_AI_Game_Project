

% -----------------------------------------------------------------------
% Predicat qui permet de connaitre les voisins directs de chaque case
% -----------------------------------------------------------------------
isNeighbor(1,[2,4]).
isNeighbor(2,[1,3,5]).
isNeighbor(3,[2,6]).
isNeighbor(4,[1,5,7]).
isNeighbor(5,[2,4,6,8]).
isNeighbor(6,[3,5,9]).
isNeighbor(7,[4,8]).
isNeighbor(8,[5,7,9]).
isNeighbor(9,[6,8]).


% -----------------------------------------------------------------------
% Tous les deplacements possibles
% ----------------------------------------------------------------------
transition(3,2,1).
transition(3,6,1).
transition(3,1,2).
transition(3,5,2).
transition(3,9,2).
transition(3,4,3).
transition(3,8,3).
transition(1,2,1).
transition(1,4,1).
transition(1,3,2).
transition(1,5,2).
transition(1,6,3).
transition(1,7,2).
transition(1,8,3).
transition(2,1,1).
transition(2,3,1).
transition(2,5,1).
transition(2,4,2).
transition(2,6,2).
transition(2,8,2).
transition(2,7,3).
transition(2,9,3).
transition(4,1,1).
transition(4,7,1).
transition(4,5,1).
transition(4,2,2).
transition(4,8,2).
transition(4,6,2).
transition(4,3,3).
transition(4,9,3).

transition(5,2,1).
transition(5,4,1).
transition(5,6,1).
transition(5,8,1).
transition(5,1,2).
transition(5,3,2).
transition(5,7,2).
transition(5,9,2).
transition(6,9,1).
transition(6,3,1).
transition(6,5,1).
transition(6,2,2).
transition(6,8,2).
transition(6,4,2).
transition(6,7,3).
transition(6,1,3).

transition(7,8,1).
transition(7,4,1).
transition(7,9,2).
transition(7,5,2).
transition(7,1,2).
transition(7,2,3).
transition(7,6,3).
transition(9,8,1).
transition(9,6,1).
transition(9,7,2).
transition(9,5,2).
transition(9,3,2).
transition(9,2,3).
transition(9,4,3).
transition(8,7,1).
transition(8,9,1).
transition(8,5,1).
transition(8,4,2).
transition(8,6,2).
transition(8,2,2).
transition(8,1,3).
transition(8,3,3).
