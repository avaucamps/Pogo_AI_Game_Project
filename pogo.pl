% Test :
% [[[1],[1,1]],[[2],[1,1]],[[3],[1,1]],[[4],[]],[[5],[]],[[6],[]],[[7],[2,2]],[[8],[2,2]],[[9],[2,2]]]
%

%------------------------------------------------------------------------
% Prédicat qui fait commencer la partie en créant le plateau de jeu
% L correspond au plateau de jeu
% -----------------------------------------------------------------------
start_game(L) :- create_list(1,L), write(L), tourJoueur1(0,L).

% -----------------------------------------------------------------------
% Prédicat qui permet de créer la liste constituant le plateau
% On instancie les 9 cases qui composent le plateau
% N correspond au numéro de la case, L correspond à la liste contenant
% d'une part une liste avec le numéro, d'autre part une liste avec les
% pions presents sur cette case
% -----------------------------------------------------------------------
create_list(10,[]).
create_list(N,[L|R]) :- instancier_case(N,L), N1 is N+1, create_list(N1,R).

%------------------------------------------------------------------------
% Prédicat qui permet d'instancier une case avec son numéro et sa liste
% de pions
% N correspond au numéro de la case, L à la liste (plateau de jeu)
%-----------------------------------------------------------------------

instancier_case(N,[L1,L2]) :- numCase(N,L1), addPionsStart(N,L2).

%------------------------------------------------------------------------
% Prédicat qui retourne le numéro d'une case
% -----------------------------------------------------------------------
numCase(N,[N]).

%------------------------------------------------------------------------
% Prédicat qui ajoute les pions sur la plateau au début de la partie
% N correspond au numéro du joueur : 1 ou 2
% -----------------------------------------------------------------------
addPionsStart(N,[1,1]) :- N < 4, !.
addPionsStart(N,[2,2]) :- N > 6, !.
addPionsStart(_,[]).

%------------------------------------------------------------------------
% Prédicat qui permet au joueur 1 de réaliser son tour
% Une fois que son tour est fini c'est au tour de l'autre joueur
% Premier paramètre = tour joué (1) ou pas (0)
% L correspond à la liste du jeu (plateau)
% X correspond à la case sur laquelle le joueur veut aller
% -----------------------------------------------------------------------
tourJoueur1(1,L) :- write(L), tourJoueur2(0,L).
tourJoueur1(0,L) :- read_start_move(X,L,1,Z), read_end_move(X,L,Z,1,L2), tourJoueur1(1,L2).

tourJoueur2(1,L) :- write(L), tourJoueur1(0,L).
tourJoueur2(0,L) :- read_start_move(X,L,2,Z), read_end_move(X,L,Z,2,L2), tourJoueur2(1,L2).

% -----------------------------------------------------------------------
% Prédicat qui permet de demander à un joueur quel mouvement il veut
% réaliser, et s'assurer que le mouvement est réalisable.
% X correspond au numéro de la case de laquelle il veut partir
% L correspond a la liste de jeu (plateau)
% N correspond au numéro du joueur
% Z2 correspond au nombre de pièces que le joueur peut bouger
% -----------------------------------------------------------------------
read_start_move(X,L,N,Z2) :-
  %isCase(X),
  askCase(L,N,X),
  infosMove(X,L,N,Z2),
  print_nbMoves(Z2).

% -----------------------------------------------------------------------
% Prédicat qui demande à un joueur de quelle case il veut partir, et
% vérifie que cela est possible.
% L = plateau de jeu
% N = numéro du joueur
% X = case de départ
% -----------------------------------------------------------------------
askCase(L,N,X):-
  write('Entrez le numéro de la case de laquelle vous voulez partir:'),
  nl,
  read(X),
  getListCase(X,L,L3),
  getListPions(L3,L2),
  isControllingCase(L2,N); askCase(L,N,X).

%------------------------------------------------------------------------
% Prédicat qui permet d'éxécuter un déplacement
% X = case de départ
% Y = case d'arrivée
% L = plateau de jeu au début du tour
% Z = nombre de déplacements maximum
% Z2 = nombre de déplacements effectués
% L2 = plateau de jeu à la fin du tour
% -----------------------------------------------------------------------
read_end_move(X,L,Z,N,L2) :-
  write('Entrez le numéro de la case dans laquelle vous voulez aller:'),
  nl,
  read(Y),
  %Pour avoir le nombre de pions à bouger on prend ce que renvoie le compteur dans checkMove
  checkMove(X,Y,Z,Z2),
  %Et on rajoute 1
  Z3 is Z2+1,
  normalize(Z3,Z4),
  makeMove(X,Y,Z4,L,L2,N).
  %isCase(X).

%------------------------------------------------------------------------
% Prédicat qui permet si le nombre passé en paramètre est un numéro de
% case valide
% -----------------------------------------------------------------------
isCase(X) :- numlist(1,9,L), member(X,L).

%------------------------------------------------------------------------
% Prédicat qui permet de savoir si le mouvement envisagé par un joueur
% est correct et donne des infos sur le futur déplacement
% X = case de laquelle le joueur veut partir
% L = plateau du jeu
% N = numéro du joueur
% Z2 = nombre de pions que le joueur peut bouger
% -----------------------------------------------------------------------
infosMove(1,[L|_],N, Z2) :- getListPions(L,L2), numberOfMovablePions(N,L2,Z), normalize(Z,Z2).
infosMove(X,[_|R],N,Z2) :- X1 is X-1, infosMove(X1,R,N,Z2).

%------------------------------------------------------------------------
% Prédicat qui permet de savoir si le joueur controle la case, c'est à
% dire que le dernier pion appartient au joueur en question
% L = plateau de jeu
% N = numéro du joueur et ses pions
% -----------------------------------------------------------------------
isControllingCase([N|_],N).

%------------------------------------------------------------------------
% Prédicat qui permet de récupérer le nombre de pions que l'on peut
% déplacer
% N = numéro du joueur et ses pions
% Z = nombre de pions déplaçables
% -----------------------------------------------------------------------
numberOfMovablePions(_,[],0).
numberOfMovablePions(N,[N|R],Z) :- numberOfMovablePions(N,R,Z1), !, Z is Z1+1.
numberOfMovablePions(N,_,Z) :- numberOfMovablePions(N,[],Z).

%------------------------------------------------------------------------
% Prédicat qui permet de récupérer les pions d'une case à partir de la
% liste correspondant à la case
% -----------------------------------------------------------------------
getListPions([_,B],B).

% -----------------------------------------------------------------------
% Prédicat qui permet de récupérer la liste d'une case, c'est a dire son
% numéro et ses pions
% N = numero de la case
% X = liste de la case
% L = plateau de jeu
% -----------------------------------------------------------------------
getListCase(1,[X|_],X).
getListCase(N,[_|R],X) :- N>1 , N1 is N-1 , getListCase(N1,R,X).

% -----------------------------------------------------------------------
% Prédicat qui renvoie la liste contenant juste le numéro de la case
% -----------------------------------------------------------------------
getCase([A,_],A).

%------------------------------------------------------------------------
% Prédicat qui permet de mettre le nombre maximum de pions déplaçables à
% trois
% -----------------------------------------------------------------------
normalize(Z,Z) :- Z < 3.
normalize(_,3).

%------------------------------------------------------------------------
% Prédicat qui permet de dire à l'utilisateur combien de mouvements il
% peut effectuer
% -----------------------------------------------------------------------
print_nbMoves(Z2) :- Z2 > 1, write("Vous pouvez effectuer de 1 a "),write(Z2),write(" deplacements.").
print_nbMoves(_) :- write("Vous pouvez effectuer 1 deplacement.").

% -----------------------------------------------------------------------
% Prédicat qui permet de connaître les voisins directs de chaque case
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
% Prédicat qui permet de vérifier si un mouvement est possible
% X = case départ
% Y = case arrivée
% Z = nombre de déplacement de 1 case possibles
% Z2 = compteur de déplacements dont la valeur est le nombre de
% déplacements -1
% -----------------------------------------------------------------------
checkMove(X,Y,_,0) :- isNeighbor(X,L),member(Y,L).
checkMove(X,Y,Z,Z2) :- Z>0, Z1 is Z-1, isNeighbor(X,L),member(A,L),checkMove(A,Y,Z1,Z3), Z2 is Z3+1.
%checkMove(_,_,Z,_):- Z==0, write('C cassee').

% -----------------------------------------------------------------------
% Prédicat qui permet d'éxécuter un déplacement
% makeMove(+X,+Y,+Z,+L,?L2).
% X = case départ
% Y = case arrivée
% Z = nombre de pions à bouger
% L = plateau de jeu avant tour
% L2 = plateau de jeu après tour
% N = numéro du joueur
% -----------------------------------------------------------------------
makeMove(X,Y,Z,L,L2,N) :- getListCase(X,L,L3), getListCase(Y,L,L4), removePionsListCase(L3,Z,L5), addPionsListCase(L4,Z,L6,N), setNewList(X,Y,L,L5,L6,L2).

% -----------------------------------------------------------------------
% Prédicat qui enlève Z pions d'une liste d'une case et retourne la
% liste sans les Z pions
% L = liste de la case
% Z = nombre de pions à enlever
% L2 = nouvelle liste
% -----------------------------------------------------------------------
removePionsListCase(L,Z,L2) :- getListPions(L,L3), removePions(L3,Z,L4), setListCase(L,L4,L2).

% -----------------------------------------------------------------------
% Prédicat qui enlève Z pions à une liste de pions L = liste début Z =
% nombre de pions L2 = liste pions fin
% -----------------------------------------------------------------------
removePions(L,0,L).
removePions([_|R], Z, L) :- Z > 0, Z1 is Z-1, removePions(R,Z1,L).

% -----------------------------------------------------------------------
% Prédicat qui met à jour le nombre de pions d'une liste correspondant à
% une case L = liste case début
% L4 = liste pions
% L2 = nouvelle liste case
% -----------------------------------------------------------------------
setListCase([A,_],L4,[A,L4]).

% -----------------------------------------------------------------------
% Prédicat qui ajoute Z pions à liste d'une case et retourne la liste
% avec les Z pions en plus
% L = liste de la case
% Z = nombre de pions à ajouter
% L2 = nouvelle liste avec les pions
% N = numéro du joueur
% ----------------------------------------------------------------------
addPionsListCase(L,Z,L2,N) :- getListPions(L,L3), addPions(L3,Z,L4,N), setListCase(L,L4,L2).

% -----------------------------------------------------------------------
% Prédicat qui ajoute Z pions à une liste de pions
% L = liste début
% Z = nombre de pions
% L2 = liste pions fin
% N = numero du joueur
% -----------------------------------------------------------------------
addPions(L,0,L,_).
addPions(L, Z, [N|R], N) :- Z > 0, Z1 is Z-1, addPions(L,Z1,R,N).

% -----------------------------------------------------------------------
% Prédicat qui permet de reconstruire le plateau de jeu après un tour en
% prenant compte des changements
% X = case départ
% Y = case arrivée
% L = plateau de jeu avant tour
% L2 = liste case départ
% L3 = liste case arrivée
% L4 = liste après le tour
% -----------------------------------------------------------------------
setNewList(0,0,L,[],[],L).
setNewList(X,Y,[A|R],L2,L3,[L2|R2]) :- getCase(A,LC), numCase(X,LC),!, setNewList(0,Y,R,[],L3,R2).
setNewList(X,Y,[A|R],L2,L3,[L3|R2]) :- getCase(A,LC), numCase(Y,LC),!, setNewList(X,0,R,L2,[],R2).
setNewList(X,Y,[A|R],L2,L3,[A|R2]) :- setNewList(X,Y,R,L2,L3,R2).
%setNewList(1,Y,[_|R],L2,L3,[L2|R2]) :- setNewList(0,Y,R,L2,L3,R2).
%setNewList(X,1,[_|R],L2,L3,[L3|R2]) :- setNewList(X,0,R,L2,L3,R2).
%setNewList(X,Y,[A|R],L2,L3,[A|R2]) :- X > 1, X1 is X-1, Y > 0, Y1 is Y
% -1, setNewList(X1,Y1,R,L2,L3,R2). setNewList(-1,Y,[_|R],L2,L3,[L2|R2])
% :- setNewList(-1,Y,R,L2,L3,R2). setNewList(X,-1,[_|R],L2,L3,[L3|R2]) :-
% setNewList(X,-1,R,L2,L3,R2).
