% Test :
% [[[1],[1,1]],[[2],[1,1]],[[3],[1,1]],[[4],[]],[[5],[]],[[6],[]],[[7],[2,2]],[[8],[2,2]],[[9],[2,2]]]
%
:- use_module(library(clpfd)).


%------------------------------------------------------------------------
% Predicat qui fait commencer la partie en creant le plateau de jeu
% L
% start_game(?L).
% L = plateau de jeu
% -----------------------------------------------------------------------
start_game(L) :- create_list(1,L), printList(L), tourJoueur1(0,L).

% -----------------------------------------------------------------------
% Predicat qui permet de savoir quel est le joueur adverse
% playerAdv(+N,?N1).
% N = numéro du joueur
% N1 = numéro du joueur adverse
% -----------------------------------------------------------------------
playerAdv(1,2).
playerAdv(2,1).

% -----------------------------------------------------------------------
% Predicat qui permet d'afficher la liste representant le plateau de
% jeu sous la forme d'une matrice 3x3
% printList(+L).
% L = liste plateau de jeu
% ----------------------------------------------------------------------
printList(L):- write('-------------------------------'),nl,afficheLigne1(L),nl,write('------------'),nl,afficheLigne2(L),nl,write('------------'),nl,afficheLigne3(L),nl.
afficheLigne1(L):- getListCase(1,L,[_,X]),write(X),write(' | '),
  getListCase(2,L,[_,Y]),write(Y),write(' | '),
  getListCase(3,L,[_,Z]),write(Z).

afficheLigne2(L):- getListCase(4,L,[_,X]),write(X),write(' | '),
  getListCase(5,L,[_,Y]),write(Y),write(' | '),
  getListCase(6,L,[_,Z]),write(Z).

afficheLigne3(L):- getListCase(7,L,[_,X]),write(X),write(' | '),
  getListCase(8,L,[_,Y]),write(Y),write(' | '),
  getListCase(9,L,[_,Z]),write(Z).

%afficheCase([]).
%afficheCase([1|R]):-write('X'),afficheCase(R).
%afficheCase([2|R]):-write('O'),afficheCase(R).

% -----------------------------------------------------------------------
% Predicat qui permet de creer la liste constituant le plateau de jeu
% On instancie les 9 cases qui composent le plateau
% create_list(+N,?L).
% N = numéro de la case
% L = liste numero de case + liste pions
% -----------------------------------------------------------------------
create_list(10,[]).
create_list(N,[L|R]) :- instancier_case(N,L), N1 is N+1, create_list(N1,R).

%------------------------------------------------------------------------
% Predicat qui permet d'instancier une case avec son numero et sa liste
% de pions
% instancierCase(+N,?L).
% N correspond au numero de la case, L a la liste (plateau de jeu)
% L = liste numero case + liste pions
%-----------------------------------------------------------------------
instancier_case(N,[L1,L2]) :- numCase(N,L1), addPionsStart(N,L2).

%------------------------------------------------------------------------
% Predicat qui retourne le numero d'une case
% numCase(?N,+L).
% N = numéro de case
% L = liste numéro de case
% -----------------------------------------------------------------------
numCase(N,[N]).

%------------------------------------------------------------------------
% Predicat qui ajoute les pions sur la plateau au debut de la partie
% addPionsStart(+N,?L).
% N = numéro joueur
% L = liste des pions
% -----------------------------------------------------------------------
addPionsStart(N,[1,1]) :- N < 4, !.
addPionsStart(N,[2,2]) :- N > 6, !.
addPionsStart(_,[]).

%------------------------------------------------------------------------
% Predicat qui permet au joueur de realiser son tour
% Une fois que son tour est fini c'est au tour de l'autre joueur
% tourJoueur(+N,+L).
% N =  si tour effectue, 0 sinon
% L = liste plateau de jeu
% -----------------------------------------------------------------------
tourJoueur1(1,L) :- player1Win(L),!.
tourJoueur1(1,L) :- printList(L), tourJoueur2(0,L).
tourJoueur1(0,L) :- read_start_move(X,L,1,Z), read_end_move(X,L,Z,1,L2), tourJoueur1(1,L2).

tourJoueur2(1,L) :- player2Win(L),!.
tourJoueur2(1,L) :- printList(L), tourJoueur1(0,L).
tourJoueur2(0,L) :- aiPlay(2,L,L2), tourJoueur2(1,L2).%read_start_move(X,L,2,Z), read_end_move(X,L,Z,2,L2), tourJoueur2(1,L2).

% -----------------------------------------------------------------------
% Predicat qui permet de demander a un joueur quel mouvement il veut
% realiser, et s'assurer que le mouvement est realisable.
% read_start_move(?X,+L,+N,?Z).
% X = numero de la case de laquelle il veut partir
% L = liste de jeu (plateau)
% N = numero du joueur
% Z = nombre de pieces que le joueur peut bouger
% -----------------------------------------------------------------------
read_start_move(X,L,N,Z2) :-
  %isCase(X),
  askCase(L,N,X),
  infosMove(X,L,N,Z2),
  print_nbMoves(Z2).

% -----------------------------------------------------------------------
% Predicat qui demande a  un joueur de quelle case il veut partir, et
% verifie que cela est possible.
% askCase(+L,+N,?X).
% L = plateau de jeu
% N = numero du joueur
% X = case de depart
% -----------------------------------------------------------------------
askCase(L,N,X):-
  write('Entrez le numÃ©ro de la case de laquelle vous voulez partir:'),
  nl,
  read(X),
  getListCase(X,L,L3),
  getListPions(L3,L2),
  isControllingCase(L2,N); askCase(L,N,X).

%------------------------------------------------------------------------
% Predicat qui permet de demande et executer un deplacement
% read_end_move(+X,?Y,+L,+Z,?Z2,+L2).
% X = case de depart
% Y = case d'arrivee
% L = plateau de jeu au debut du tour
% Z = nombre de deplacements maximum
% Z2 = nombre de deplacements effectues
% L2 = plateau de jeu a la fin du tour
% -----------------------------------------------------------------------
read_end_move(X,L,Z,N,L2) :-
  write('Entrez le numÃ©ro de la case dans laquelle vous voulez aller:'),
  nl,
  read(Y),
  %Pour avoir le nombre de pions Ã  bouger on prend ce que renvoie le compteur dans checkMove
  checkMove(X,Y,Z,Z2),
  %Et on rajoute 1
  Z3 is Z2+1,
  normalize(Z3,Z4),
  makeMove(X,Y,Z4,L,L2,N).
  %isCase(X).

%------------------------------------------------------------------------
% Predicat qui permet de savoir si le nombre passe en parametre est un
% numero de case valide
% -----------------------------------------------------------------------
isCase(X) :- numlist(1,9,L), member(X,L).

%------------------------------------------------------------------------
% Predicat qui permet de savoir si le mouvement envisage par un joueur
% est correct et donne des infos sur le futur deplacement
% infosMove(+X,+L,+N,?Z)
% X = case de laquelle le joueur veut partir
% L = plateau du jeu
% N = numero du joueur
% Z = nombre de pions que le joueur peut bouger
% -----------------------------------------------------------------------
infosMove(1,[L|_],N, Z2) :- getListPions(L,L2), numberOfMovablePions(N,L2,Z), normalize(Z,Z2).
infosMove(X,[_|R],N,Z2) :- X1 is X-1, infosMove(X1,R,N,Z2).

%------------------------------------------------------------------------
% Predicat qui permet de savoir si le joueur controle la case, c'est a 
% dire si le dernier pion appartient au joueur en question
% isControllingCase(+L,?N).
% L = plateau de jeu
% N = numero du pion se trouvant au dessus de la pile
% -----------------------------------------------------------------------
isControllingCase([],0).
isControllingCase([N|_],N).

% -----------------------------------------------------------------------%Permet
% de savoir si un joueur controle la case
% ----------------------------------------------------------------------
ownsCase([N|_],N).


%------------------------------------------------------------------------
% Predicat qui permet de recuperer le nombre de pions que l'on peut
% deplacer
% numberOfMovablePions(+N,+L,?Z).
% N = numero du joueur et ses pions
% L = liste des pions
% Z = nombre de pions deplacables
% -----------------------------------------------------------------------
numberOfMovablePions(_,[],0).
numberOfMovablePions(N,[N|R],Z) :- numberOfMovablePions(N,R,Z1), !, Z is Z1+1.
numberOfMovablePions(N,_,Z) :- numberOfMovablePions(N,[],Z).

%------------------------------------------------------------------------
% Predicat qui permet de recuperer les pions d'une case a partir de
% la liste correspondant a la case
% getListPions(+L,?P).
% L = liste de la case
% P = liste des pions
% ----------------------------------------------------------------------
getListPions([_,B],B).

% -----------------------------------------------------------------------
% Predicat qui permet de recuperer la liste d'une case, c'est a dire
% son numero et ses pions
% getListCase(+N,?X,+L2).
% N = numero de la case
% X = liste de la case
% L = plateau de jeu
% -----------------------------------------------------------------------
getListCase(1,[X|_],X).
getListCase(N,[_|R],X) :- N>1 , N1 is N-1 , getListCase(N1,R,X).

% -----------------------------------------------------------------------
% Prdicat qui renvoie la liste contenant juste le numero de la case
% -----------------------------------------------------------------------
getCase([A,_],A).

%------------------------------------------------------------------------
% Predicat qui permet de mettre le nombre maximum de pions deplacables
% a trois
% -----------------------------------------------------------------------
normalize(Z,Z) :- Z < 3.
normalize(_,3).

%------------------------------------------------------------------------
% Predicat qui permet de dire a l'utilisateur combien de mouvements il
% peut effectuer
% -----------------------------------------------------------------------
print_nbMoves(Z2) :- Z2 > 1, write("Vous pouvez effectuer de 1 a "),write(Z2),write(" deplacements.").
print_nbMoves(_) :- write("Vous pouvez effectuer 1 deplacement.").

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
% Predicat qui permet de verifier si un mouvement est possible
% checkMove(+X,+Y,+Z,?Z2).
% X = case depart
% Y = case arrivee
% Z = nombre de deplacements de 1 case possibles
% Z2 = compteur de deplacements dont la valeur est le nombre de
% deplacements -1
% -----------------------------------------------------------------------
checkMove(X,Y,_,0) :- isNeighbor(X,L),member(Y,L).
checkMove(X,Y,Z,Z2) :- Z>0, Z1 is Z-1, isNeighbor(X,L),member(A,L),checkMove(A,Y,Z1,Z3), Z2 is Z3+1.
%checkMove(_,_,Z,_):- Z==0, write('C cassee').

% -----------------------------------------------------------------------
% Predicat qui permet d'executer un deplacement
% makeMove(+X,+Y,+Z,+L,?L2).
% X = case depart
% Y = case arrivee
% Z = nombre de pions a bouger
% L = plateau de jeu avant tour
% L2 = plateau de jeu apres tour
% N = numero du joueur
% -----------------------------------------------------------------------
makeMove(X,Y,Z,L,L2,N) :- getListCase(X,L,L3), getListCase(Y,L,L4), removePionsListCase(L3,Z,L5), addPionsListCase(L4,Z,L6,N), setNewList(X,Y,L,L5,L6,L2).

% -----------------------------------------------------------------------
% Predicat qui enleve Z pions d'une liste d'une case et retourne la
% liste sans les Z pions
% removePionsListCase(+L,+Z,?L2).
% L = liste de la case
% Z = nombre de pions Ã  enlever
% L2 = nouvelle liste
% -----------------------------------------------------------------------
removePionsListCase(L,Z,L2) :- getListPions(L,L3), removePions(L3,Z,L4), setListCase(L,L4,L2).

% -----------------------------------------------------------------------
% Predicat qui enleve Z pions a une liste de pions
% removePions(+L,+Z,?L2).
% L = liste debut
% Z = nombre de pions
% L2 = liste pions fin
% -----------------------------------------------------------------------
removePions(L,0,L).
removePions([_|R], Z, L) :- Z > 0, Z1 is Z-1, removePions(R,Z1,L).

% -----------------------------------------------------------------------
% Predicat qui met a jour le nombre de pions d'une liste correspondant
% a une case
% setListCase(+L,+L4,?L2).
% L = liste case debut
% L4 = liste pions
% L2 = nouvelle liste case
% -----------------------------------------------------------------------
setListCase([A,_],L4,[A,L4]).

% -----------------------------------------------------------------------
% Predicat qui ajoute Z pions a liste d'une case et retourne la liste
% avec les Z pions en plus
% addPionsListCase(+L,+Z,?L2,+N).
% L = liste de la case
% Z = nombre de pions Ã  ajouter
% L2 = nouvelle liste avec les pions
% N = numero du joueur
% ----------------------------------------------------------------------
addPionsListCase(L,Z,L2,N) :- getListPions(L,L3), addPions(L3,Z,L4,N), setListCase(L,L4,L2).

% -----------------------------------------------------------------------
% Predicat qui ajoute Z pions a une liste de pions
% addPions(+L,+Z,?L2,+N).
% L = liste debut
% Z = nombre de pions
% L2 = liste pions fin
% N = numero du joueur
% -----------------------------------------------------------------------
addPions(L,0,L,_).
addPions(L, Z, [N|R], N) :- Z > 0, Z1 is Z-1, addPions(L,Z1,R,N).

% -----------------------------------------------------------------------
% Predicat qui permet de reconstruire le plateau de jeu apres un tour en
% prenant compte des changements
% setNewList(+X,+Y,+L,+L2,+L3,?L4).
% X = case depart
% Y = case arrivee
% L = plateau de jeu avant tour
% L2 = liste case depart
% L3 = liste case arrivee
% L4 = liste apres le tour
% -----------------------------------------------------------------------
setNewList(0,0,L,[],[],L).
setNewList(X,Y,[A|R],L2,L3,[L2|R2]) :- getCase(A,LC), numCase(X,LC),!, setNewList(0,Y,R,[],L3,R2).
setNewList(X,Y,[A|R],L2,L3,[L3|R2]) :- getCase(A,LC), numCase(Y,LC),!, setNewList(X,0,R,L2,[],R2).
setNewList(X,Y,[A|R],L2,L3,[A|R2]) :- setNewList(X,Y,R,L2,L3,R2).

% -----------------------------------------------------------------------
% Predicat qui compte le nombre de piles controlees par un joueur passe
% en param
% nbPilesPlayer(+N,+L,?X).
% N = numero du joueur
% L = plateau de jeu
% X = nombre de piles
% -----------------------------------------------------------------------
nbPilesPlayer(_,[],0).
nbPilesPlayer(N,[L|R],X) :- getListPions(L,L2), colorLastPion(L2,N), !, nbPilesPlayer(N,R,X1), X is X1+1.
nbPilesPlayer(N,[_|R],X) :- nbPilesPlayer(N,R,X).

% ------------------------------------------------------------------------
% Predicat qui permet de connaitre la couleur du pion sur le dessus
% d'une pile
% colorLastPion(+L,?C).
% L = liste des pions
% C = 1 pour couleur joueur 1, 2 pour couleur joueur 2, 0 si liste vide
% -----------------------------------------------------------------------
colorLastPion([],0).
colorLastPion([X|_],X).

% -----------------------------------------------------------------------
% Predicat qui calcul nbCasesIA - nbCasesPlayer pour savoir quel coup
% faire
% Renvoie 10 si le joueur N a gagne
% diffCasesPlayer2(+N,+L,?X).
% N = numéro du joueur
% L = plateau de jeu
% X = nbCasesIA - nbCasesPlayer
% -----------------------------------------------------------------------
diffCasesPlayer(_,[],_).
diffCasesPlayer(2,L,10) :- nbPilesPlayer(1,L,X), X == 0, !.
diffCasesPlayer(1,L,10) :- nbPilesPlayer(2,L,X), X == 0, !.
diffCasesPlayer(2,L,-10) :- nbPilesPlayer(2,L,X), X == 0, !.
diffCasesPlayer(1,L,-10) :- nbPilesPlayer(1,L,X), X == 0, !.
diffCasesPlayer(1,L,X) :- nbPilesPlayer(1,L,Y), nbPilesPlayer(2,L,Z), X is Y-Z.
diffCasesPlayer(2,L,X) :- nbPilesPlayer(2,L,Z), nbPilesPlayer(1,L,Y), X is Z-Y.

% -----------------------------------------------------------------------
% Predicat qui permet de calculer les heuristiques pour le joueur 2 pour
% chaque état de la liste des états possibles. Renvoie la liste de
% toutes les heurstiques
% calculateHeurisitics(+N,+LE,?LH)
% N = numéro du joueur
% LE = liste des états
% LH = liste des heuristiques
% -----------------------------------------------------------------------
calculateHeuristics(_,[],[]).
calculateHeuristics(N,[L|R],[L2|R2]) :- calculateHeuristicsList(N,L,L2), calculateHeuristics(N,R,R2).

% Meme predicat que precedemmment qui renvoie la liste des heuristiques
% des etats L
calculateHeuristicsList(_,[],[]).
calculateHeuristicsList(N,[L|R],[H|R2]) :- diffCasesPlayer(N,L,H), calculateHeuristicsList(N,R,R2).

%Prédicat qui renvoie directement l'heuristique de la liste L
calculateHeuristicSimple(N, L, H) :- diffCasesPlayer(N,L,H).


% -----------------------------------------------------------------------
% Predicat qui permet de calculer la meilleure solution à jouer
% minimax(+N,+L,?L2,+I).
% N = numéro du joueur
% L = liste plateau de jeu
% L2 = liste après tour suivant
% Profondeur de recherche
% -----------------------------------------------------------------------
aiPlay(N,L,L2) :- getAllMovesPlayer(N,L,LE), write(LE).%, concatLists(LE,L3,L4),deleteLastElementList(L3,L5).%minimax(N,LE,I), flatten(I,I2).%, indexMax(I2,Z,Z2), getList(LE,Z2,L2).


concatLists([],_).
concatLists([L|R],L2) :- append(L,L2,L3), concatLists(R,L3).

deleteLastElementList([X|List], NewList) :- deleteLastElementList2(List, NewList, X).
deleteLastElementList2([], [], _).
deleteLastElementList2([X1|R], [X0|R2], X0) :-
   deleteLastElementList2(R, R2, X1).

minimax(_,[],[]).
minimax(N,[L|R],[I|R2]) :- minimax_2(N,L,I), minimax(N,R,R2).

%minimax3(_,[],[]).
% minimax3(N,[L|R],[I|R2]) :- playerAdv(N,N2),
% getAllMovesPlayer(N2,L,LH2), minimax4(N,LH2,LH3), flatten(LH3,LH4),
% listMin(LH4,I), minimax3(N,R,R2).

%minimax4(_,[],[]).
%minimax4(N,[L|R],[I|R2]) :- minimax5(N,L,I), minimax4(N,R,R2).

%minimax5(_,[],[]).
% minimax5(N,[L|R],[I|R2]) :- getAllMovesPlayer(N,L,LH2),
% minimax6(N,LH2,LH3), flatten(LH3,LH4), listMax(LH4,I),
% minimax5(N,R,R2).

%minimax6(_,[],[]).
%minimax6(N,[L|R],[I|R2]) :- minimax2(N,L,I), minimax6(N,R,R2).

minimax_2(_,[],[],_).
% minimax_2(N,[L|R],[I|R2], Min) :-
% (getAllMovesPlayerHeuristics(N,L,LH,Min), listMin(LH,I), ((I>Min,
% minimax_2(N,R,R2,I));minimax_2(N,R,R2,I)); minimax_2(N,R,R2,Min)).
minimax_2(N,[L|R],[I|R2],Min) :- getAllMovesPlayerHeuristics(N,L,LH,Min), !, listMin(LH,I), minimax_2(N,R,R2,I).
minimax_2(N,[_|R],[-100|R2],Min) :- minimax_2(N,R,R2,Min).

%minimax2(_,[],[]).
% minimax2(N,[L|R],[I|R2]) :- playerAdv(N,N2),
% getAllMovesPlayerHeuristics(N2,L,LH2), minimax2(N,R,R2).

%------------------------------------------------------------------------
%A voir
% -----------------------------------------------------------------------
getList([L|_],I,L2) :- length(L,A), A >= I, getList2(L,I,L2).
getList([L|R],I,L2) :- length(L,A), A < I, I1 = I-A, getList(R,I1,L2).
getList2([L|_],0,L).
getList2([_|R],I,L2) :- I1 is I-1, getList2(R,I1,L2).

%Predicat qui retourne l'index du plus grand nombre de la liste
indexMax(Zs,Max,Pos) :-
   maplist(#>=(Max),Zs),
   nth0(Pos,Zs,Max).

%Predicat qui renvoie le plus grand nombre de la liste L
listMax([L|R], Max) :- foldl(numMax,R,L,Max).
numMax(X,Y,Max) :- Max #= max(X,Y).

%Predicat qui renvoie le plus petit nombre de la liste L
listMin([L|R], Min) :- foldl(numMin, R, L, Min).
numMin(X, Y, Min) :- Min #= min(X, Y).

% -----------------------------------------------------------------------
% Prdicat qui permet de recuperer tous les etats du plateau possibles
% apres le coup qui va intervenir en integrant minimax avec alpha-beta
% getAllMovesPlayerHeuristics(+N,+L,?LE).
% N = numero du joueur
% L = plateau de jeu
% LE = liste de tous les etats possibles
% On cherche d'abord le minimum de toutes les solutions pour un
% mouvement a partir de l'etat actuel depuis la premiere case que
% controle le joueur N. On cherche ensuite pour chaque autre case le
% coup minimum possible en appliquant l'elagage alpha beta : on compare
% les valeurs trouvees au minimum trouve precedemment, si c'est
% inferieur on coupe la branche, sinon on la garde
% -----------------------------------------------------------------------
% getAllMovesPlayerHeuristicsss(N,L,LH5) :- playerAdv(N,N2),
% getCasesPlayer(L,LC,N2), firstElement(LC,C1),
% getAllMovesFirst([C1],L,LE,N2), write(LE),
% calculateHeuristicsList(N,LE,LH), write(LH), flatten(LH,LH2),
% listMin(LH2,Min), deleteFirstElement(LC,LC2),
% getAllMoves(LC2,L,LE2,N2,Min), calculateHeuristics(N,LE2,LH3),
% flatten(LH3,LH4), addMin(Min,LH4,LH5).

getAllMovesPlayerHeuristics(N,L,LH2,Min) :- playerAdv(N,N2), getCasesPlayer(L,LC,N), getAllMoves3(LC,L,LE,N,Min), calculateHeuristics(N2,LE,LH), flatten(LH,LH2).


% Predicat qui permet d'ajouter un element en premiere position d'une
% liste
%addMin(Min,L,[Min|L]).

%Predicat qui retourne le premier element d'une liste
%firstElement([A|_],A).

%Predicat qui supprime le dernier element d'une liste
%deleteFirstElement([_|R],R).

% Predict qui retourne tous les etats possibles a partir de l'etat
% actuel (sans alpha-beta)
getAllMovesPlayer(N,L,LE) :-getCasesPlayer(L,LC,N), getAllMoves2(LC,L,LE,N), playerAdv(N,N2), calculateHeuristics(N2,LE,LH2), write(LH2).

% -----------------------------------------------------------------------
% Predicat qui permet de recuperer la liste des cases controlees
% par le joueur N.
% getCasesPlayer(+L,?L2,+N).
% L = plateau de jeu
% L2 = liste des cases controlees par le joueur N (case = liste
% comprenant liste num case + liste pions case)
% N = numÃ©ro du joueur
% -----------------------------------------------------------------------
getCasesPlayer([],[],_).
getCasesPlayer([C|R],[C|R2],N) :- getListPions(C,LP), ownsCase(LP,N),getCasesPlayer(R,R2,N).
getCasesPlayer([C|R],L2,N) :- getListPions(C,LP), not(ownsCase(LP,N)), getCasesPlayer(R,L2,N).

% -----------------------------------------------------------------------
% Predicat qui permet d'avoir tous les etats possibles pour le premier
% etat pour la premiere case
% getAllMoves(+LC,+LP,?LE,+N).
% LC = liste des cases controlees par le joueur
% LP = liste correspondant au plateau LE = liste des etats possibles
% N = numero du joueur
% -----------------------------------------------------------------------
% getAllMovesFirst([],_,[],_).
% getAllMovesFirst([C],LP,LE,N) :- getCase(C,LC), numCase(NC,LC),
% getListPions(C,LP2), numberOfMovablePions(N,LP2,Z),
% getMovesFromCases(NC,L2,Z), flatten(L2,L),
% generateAllStates(NC,L,LP,LE,N).%, getAllMovesFirst([],LP,R2,N).

% Predicat qui permet d'avoir tous les mouvement possibles a partir
% d'une list de case en appliquant minimax + alpha beta
% getAllMoves([],_,[],_,_).
% getAllMoves([C|R],LP,[LE|R2],N,Min) :- getCase(C,LC), numCase(NC,LC),
% getListPions(C,LP2), numberOfMovablePions(N,LP2,Z),
% getMovesFromCases(NC,L2,Z), flatten(L2,L),
% ((generateAllStatesMin(NC,L,LP,LE,N,Min), playerAdv(N,N2),
% calculateHeuristicsList(N2,LE,LH), flatten(LH,LH2), listMin(LH2,Min2),
% getAllMoves(R,LP,R2,N,Min2)); getAllMoves(R,LP,R2,N,Min)).

getAllMoves3([],_,[],_,_).
getAllMoves3([C|R],LP,[LE|R2],N,Min) :- getCase(C,LC), numCase(NC,LC), getListPions(C,LP2), numberOfMovablePions(N,LP2,Z), getMovesFromCases(NC,L2,Z), flatten(L2,L), generateAllStatesMin(NC,L,LP,LE,N,Min), getAllMoves3(R,LP,R2,N,Min).

% Predicat qui permet d'avoir tous les mouvements possibles a partir
% d'une liste de cases
getAllMoves2([],_,[],_).
getAllMoves2([C|R],LP,[LE|R2],N) :- getCase(C,LC), numCase(NC,LC), getListPions(C,LP2), numberOfMovablePions(N,LP2,Z), getMovesFromCases(NC,L2,Z), flatten(L2,L), generateAllStates(NC,L,LP,LE,N), getAllMoves2(R,LP,R2,N).
% -----------------------------------------------------------------------
% Predicat qui permet de generer tous les etats possibles d'une case
% de depart a plusieurs cases d'arrivees
% generateAllStates(C,LC,LJ,LJ2,N).
% C = case de depart
% LC = liste des cases d'arrivee
% LJ = Liste correspondant au plateau
% LJ2 = liste correspondant aux etats possibles a  partir de la case de
% depart et des cases d'arrivees
% N = numero du joueur
% On intègre directement alpha-beta : si min joue, si la valeur la plus
% petite n'est pas dépassée on termine les recherches pour la case de
% départ C Si max joue, si la valeur max n'est pas dépassée on termine
% les recherches pour la case de départ C
% -----------------------------------------------------------------------
generateAllStates(_,[],_,[],_).
generateAllStates(C,[C2|R],LJ,[LJ2|R2],N) :- generateState(C,C2,LJ,LJ2,N), generateAllStates(C,R,LJ,R2,N).

% Predicat qui retourne les etats possibles seulement s'ils ne sont pas
% inferieur au minimum deja trouve
generateAllStatesMin(_,[],_,[],_,_).
generateAllStatesMin(C,[C2|R],LJ,[LJ2|R2],N,Min) :- generateStateMin(C,C2,LJ,LJ2,N,Min), generateAllStatesMin(C,R,LJ,R2,N,Min).


%generateAllStatesMin(_,[],_,[],_,_,_).
% generateAllStatesMin(C,[C2|R],LJ,[LJ2|R2],N,ValeurMin,[H|R3]) :-
% generateState(C,C2,LJ,LJ2,N), calculateHeuristicSimple(N,LJ2,H),
% H<ValeurMin, ValeurMin is H,
% generateAllStatesMin(C,R,LJ,R2,N,ValeurMin,R3).
% generateAllStatesMin(C,[C2|_],LJ,[LJ2|_],N,ValeurMin,[H|_]) :-
% generateState(C,C2,LJ,LJ2,N), calculateHeuristicSimple(N,LJ2,H),
% H>ValeurMin, generateAllStatesMin(C,[],LJ,[],_,_,_).

%generateAllStatesMax(_,[],_,[],_,_,_).
% generateAllStatesMax(C,[C2|R],LJ,[LJ2|R2],N,ValeurMax,[H|R2]) :-
% generateState(C,C2,LJ,LJ2,N), calculateHeuristicSimple(N,LJ2,H),
% H>ValeurMax, ValeurMax is H,
% generateAllStatesMax(C,R,LJ,R2,N,ValeurMax,R2).
% generateAllStatesMax(C,[C2|_],LJ,[LJ2|_],N,ValeurMax,[H|_]) :-
% generateState(C,C2,LJ,LJ2,N), calculateHeuristicSimple(N,LJ2,H),
% H<ValeurMax, generateAllStatesMax(C,[],LJ,[],_,_,_).
% ------------------------------------------------------------------------
% Predicat qui permet de generer l'etat possible a partir d'une case
% de depart et d'une case d'arrivee.
% generateState(+C,+C2,+LJ,?LJ2,+N).
% C = case de depart
% C2 = case arrivee
% LJ = Liste du plateau de jeu
% LJ2 = liste etat suivant
% N = numero du joueur
% -----------------------------------------------------------------------
generateState(C,C2,LJ,LJ2,N) :- transition(C,C2,NP), makeMove(C,C2,NP,LJ,LJ2,N).

generateStateMin(C,C2,LJ,LJ2,N,Min) :- playerAdv(N,N2), transition(C,C2,NP), makeMove(C,C2,NP,LJ,LJ2,N), calculateHeuristicSimple(N2,LJ2,H), H>=Min.

% -----------------------------------------------------------------------
% Predicat qui permet de savoir sur quelles cases on peut aller en
% partant d'une case avec un certain nombre de deplacements N
% getMovesFromCases(+X,?L,+N).
% X = case de depart
% L = liste des cases d'arrivee
% N = nombre de deplacements
% -----------------------------------------------------------------------
getMovesFromCases(X,[L|R],N) :- N>1, findall(Y,transition(X,Y,N),L), N1 is N-1, getMovesFromCases(X,R,N1).
getMovesFromCases(X,[L],1) :- findall(Y,transition(X,Y,1),L).

% -----------------------------------------------------------------------
% Tous les deplacements possibles
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

vide([]).

premier([A|_],A).

player1Win(L):-
getListCase(1,L,[_,M]),isControllingCase(M,A),(A\=2;vide(A)),
getListCase(2,L,[_,N]),isControllingCase(N,B),(B\=2;vide(B)),
getListCase(3,L,[_,O]),isControllingCase(O,C),(C\=2;vide(C)),
getListCase(4,L,[_,P]),isControllingCase(P,D),(D\=2;vide(D)),
getListCase(5,L,[_,Q]),isControllingCase(Q,E),(E\=2;vide(E)),
getListCase(6,L,[_,R]),isControllingCase(R,F),(F\=2;vide(F)),
getListCase(7,L,[_,S]),isControllingCase(S,G),(G\=2;vide(G)),
getListCase(8,L,[_,T]),isControllingCase(T,H),(H\=2;vide(H)),
getListCase(9,L,[_,U]),isControllingCase(U,I),(I\=2;vide(I)),
write('Joueur 1 GAGNE !').


player2Win(L):-
getListCase(1,L,[_,M]),isControllingCase(M,A),(A\=1;vide(A)),
getListCase(2,L,[_,N]),isControllingCase(N,B),(B\=1;vide(B)),
getListCase(3,L,[_,O]),isControllingCase(O,C),(C\=1;vide(C)),
getListCase(4,L,[_,P]),isControllingCase(P,D),(D\=1;vide(D)),
getListCase(5,L,[_,Q]),isControllingCase(Q,E),(E\=1;vide(E)),
getListCase(6,L,[_,R]),isControllingCase(R,F),(F\=1;vide(F)),
getListCase(7,L,[_,S]),isControllingCase(S,G),(G\=1;vide(G)),
getListCase(8,L,[_,T]),isControllingCase(T,H),(H\=1;vide(H)),
getListCase(9,L,[_,U]),isControllingCase(U,I),(I\=1;vide(I)),
write('Joueur 2 GAGNE !').
