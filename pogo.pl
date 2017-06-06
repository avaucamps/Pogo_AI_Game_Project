% Test :
% [[[1],[1,1]],[[2],[1,1]],[[3],[1,1]],[[4],[]],[[5],[]],[[6],[]],[[7],[2,2]],[[8],[2,2]],[[9],[2,2]]]
%

%------------------------------------------------------------------------
% Prédicat qui fait commencer la partie en créant le plateau de jeu
% L correspond au plateau de jeu
% -----------------------------------------------------------------------
start_game(L) :- create_list(1,L), printList(L), tourJoueur1(0,L).

% -----------------------------------------------------------------------
% Prédicat qui permet d'afficher la liste représentant le plateau de jeu
% sous la forme d'une matrice 3x3
% -----------------------------------------------------------------------
printList(L):- afficheLigne1(L),nl,write('-------'),nl,afficheLigne2(L),nl,write('-------'),nl,afficheLigne3(L),nl.
afficheLigne1(L):- getListCase(1,L,[_,X]),afficheCase(X),write(' | '),
  getListCase(2,L,[_,Y]),afficheCase(Y),write(' | '),
  getListCase(3,L,[_,Z]),afficheCase(Z).

afficheLigne2(L):- getListCase(4,L,[_,X]),afficheCase(X),write(' | '),
  getListCase(5,L,[_,Y]),afficheCase(Y),write(' | '),
  getListCase(6,L,[_,Z]),afficheCase(Z).

afficheLigne3(L):- getListCase(7,L,[_,X]),afficheCase(X),write(' | '),
  getListCase(8,L,[_,Y]),afficheCase(Y),write(' | '),
  getListCase(9,L,[_,Z]),afficheCase(Z).

afficheCase([]).
afficheCase([1|R]):-write('X'),afficheCase(R).
afficheCase([2|R]):-write('O'),afficheCase(R).

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
tourJoueur1(1,L) :- printList(L), tourJoueur2(0,L).
tourJoueur1(0,L) :- read_start_move(X,L,1,Z), read_end_move(X,L,Z,1,L2), tourJoueur1(1,L2).

tourJoueur2(1,L) :- printList(L), tourJoueur1(0,L).
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

ownsCase([N|_],N).


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

% -----------------------------------------------------------------------
% Prédicat qui compte le nombre de piles contrôlées par un joueur passé
% en param nbPilesPlayer(+N,+L,?X).
% N = numéro du joueur L = plateau de jeu
% X = nombre de piles
% -----------------------------------------------------------------------
nbPilesPlayer(_,[],0).
nbPilesPlayer(N,[L|R],X) :- getListPions(L,L2), colorLastPion(L2,N), !, nbPilesPlayer(N,R,X1), X is X1+1.
nbPilesPlayer(N,[_|R],X) :- nbPilesPlayer(N,R,X).

% ------------------------------------------------------------------------
% Prédicat qui permet de connaître la couleur du pion sur le dessus
% d'une pile
% colorLastPion(+L,?C).
% L = liste des pions
% C = 1 pour couleur joueur 1, 2 pour couleur joueur 2, 0 si liste vide
% -----------------------------------------------------------------------
colorLastPion([],0).
colorLastPion([X|_],X).

% -----------------------------------------------------------------------
% Prédicat qui calcul nbCasesIA - nbCasesPlayer pour savoir quel coup
% faire
% Renvoie 10 si l'IA a gagné
% diffCasesPlayer2(+L,?X).
% L = plateau de jeu
% X = nbCasesIA - nbCasesPlayer
% -----------------------------------------------------------------------
diffCasesPlayer2([],_).
diffCasesPlayer2(L,10) :- nbPilesPlayer(1,L,X), X == 0, !.
diffCasesPlayer2(L,X) :- nbPilesPlayer(1,L,Y), nbPilesPlayer(2,L,Z), X is Z-Y.

% -----------------------------------------------------------------------
% Prédicat qui permet de récupérer tous les états du plateau possibles
% après le coup qui va intervenir
% getAllMovesPlayer(+N,+L,?LE).
% N = numéro du joueur
% L = plateau de jeu
% LE = liste de tous les états possibles
% -----------------------------------------------------------------------
getAllMovesPlayer(N,L,LE) :-getCasesPlayer(L,LC,N), getAllMoves(LC,L,LE,N), write(LE).

% -----------------------------------------------------------------------
% Prédicat qui permet de récupérer la liste des cases contrôlées par le
% joueur N.
% getCasesPlayer(+L,?L2,+N).
% L = plateau de jeu
% L2 = liste des cases contrôlées par le joueur N (case = liste
% comprenant liste num case + liste pions case)
% N = numéro du joueur
% -----------------------------------------------------------------------
getCasesPlayer([],[],_).
getCasesPlayer([C|R],[C|R2],N) :- getListPions(C,LP), ownsCase(LP,N),getCasesPlayer(R,R2,N).
getCasesPlayer([C|R],L2,N) :- getListPions(C,LP), not(ownsCase(LP,N)), getCasesPlayer(R,L2,N).

% ------------------------------------------------------------------------
% Prédicat qui permet d'avoir tous les états possibles après le prochain
% tour à partir de la liste des cases contrôlées par le joueur.
% getAllMoves(+LC,+LP,?LE,+N).
% LC = liste des cases contrôlées par le joueur
% LP = liste correspondant au plateau
% LE = liste des états possibles
% N = numéro du joueur
% -----------------------------------------------------------------------
getAllMoves([],_,_,_).
getAllMoves([C|R],LP,[LE|R],N) :- getCase(C,LC), numCase(NC,LC), getMovesFromCases(NC,L2,N), flatten(L2,L), generateAllStates(NC,L,LP,LE,N), getAllMoves(R,LP,R,N).

% ------------------------------------------------------------------------
% Prédicat qui permet de générer tous les états possibles d'une case de
% départ à plusieurs cases d'arrivées
% generateAllStates(C,LC,LJ,LJ2,N).
% C = case de départ
% LC = liste des cases d'arrivée
% LJ = Liste correspondant au plateau
% LJ2 = liste correspondant aux états possibles à partir de la case de
% départ et des cases d'arrivées
% N = numéro du joueur
% -----------------------------------------------------------------------
generateAllStates(_,[],_,_,_).
generateAllStates(C,[C2|R],LJ,[LJ2|R2],N) :- generateState(C,C2,LJ,LJ2,N), write(LJ2), generateAllStates(C,R,LJ,R2,N).

% ------------------------------------------------------------------------
% Prédicat qui permet de générer l'état possible à partir d'une case
% de départ et d'une case d'arrivée.
% generateState(+C,+C2,+LJ,?LJ2,+N).
% C = case de départ
% C2 = case arrivée
% LJ = Liste du plateau de jeu
% LJ2 = liste état suivant
% N = numéro du joueur
% -----------------------------------------------------------------------
generateState(C,C2,LJ,LJ2,N) :- transition(C,C2,NP), makeMove(C,C2,NP,LJ,LJ2,N).

% -----------------------------------------------------------------------
% Prédicat qui permet de savoir sur quelles cases on peut aller en
% partant d'une case avec un certain nombre de déplacements N
% getMovesFromCases(+X,?L,+N).
% X = case de départ
% L = liste des cases d'arrivée
% N = nombre de déplacements
% -----------------------------------------------------------------------
getMovesFromCases(X,[L|R],N) :- N>1, findall(Y,transition(X,Y,N),L), N1 is N-1, getMovesFromCases(X,R,N1).
getMovesFromCases(X,[L],1) :- findall(Y,transition(X,Y,1),L).

% -----------------------------------------------------------------------
% Tous les déplacements possibles
% -----------------------------------------------------------------------
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