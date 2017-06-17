% Test :
% [[[1],[1,1]],[[2],[1,1]],[[3],[1,1]],[[4],[]],[[5],[]],[[6],[]],[[7],[2,2]],[[8],[2,2]],[[9],[2,2]]]
%
:- use_module(library(clpfd)).
:- consult('transitions.pl').
:- consult(display).


%Jeu utilisateur vs IA
pogo(1) :- start_game().
%Jeu IA vs IA
pogo(2) :- start_game_ia().

%------------------------------------------------------------------------
% Predicat qui fait commencer la partie en creant le plateau de jeu
% L
% start_game(?L).
% L = plateau de jeu
% -----------------------------------------------------------------------
start_game() :- create_list(1,L), printList(L), tourJoueur1(0,L).

% Predicat similaire au precedent permettant de demarrer une partie ia
% contre ia
start_game_ia() :- create_list(1,L), printList(L), tourJoueur1IA(0,L).

% -----------------------------------------------------------------------
% Predicat qui permet de savoir quel est le joueur adverse
% playerAdv(+N,?N1).
% N = numéro du joueur
% N1 = numéro du joueur adverse
% -----------------------------------------------------------------------
playerAdv(1,2).
playerAdv(2,1).

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
% Predicats qui permettent de gerer les tours lors d'un match joueur vs
% IA
% tourJoueur(+N,+L).
% N = etat du tour -> 0 si pas termine
%                  -> 1 si termine
% L = liste representant le plateau de jeu
% -----------------------------------------------------------------------
tourJoueur1(1,L) :- player1Win(L),!.
tourJoueur1(1,L) :- printList(L), write("Tour joueur 2 : "), nl, tourJoueur2(0,L).
tourJoueur1(0,L) :- read_start_move(X,L,1,Z), read_end_move(X,L,Z,1,L2), tourJoueur1(1,L2).

tourJoueur2(1,L) :- player2Win(L),!.
tourJoueur2(1,L) :- printList(L), write("Tour joueur 1 : "), nl, tourJoueur1(0,L).
tourJoueur2(0,L) :- aiPlay(2,L,L2), tourJoueur2(1,L2).

%------------------------------------------------------------------------
% Predicats qui permettent de gerer les tours lors d'un match ia vs
% IA
% tourJoueur(+N,+L).
% N = etat du tour -> 0 si pas termine
%                  -> 1 si termine
% L = liste representant le plateau de jeu
% -----------------------------------------------------------------------
tourJoueur1IA(1,L) :- player1Win(L),!.
tourJoueur1IA(1,L) :- printList(L), tourJoueur2IA(0,L).
tourJoueur1IA(0,L) :- write("Tour joueur 1 : "), nl, aiPlay(1,L,L2), tourJoueur1IA(1,L2).

tourJoueur2IA(1,L) :- player2Win(L),!.
tourJoueur2IA(1,L) :- printList(L), tourJoueur1IA(0,L).
tourJoueur2IA(0,L) :- write("Tour joueur 2 : "), nl, aiPlay(2,L,L2), tourJoueur2IA(1,L2).

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
  write('Entrez le numero de la case de laquelle vous voulez partir:'),
  nl,
  read(X),
  getListCase(X,L,L3),
  getListPions(L3,L2),
  (isControllingCase(L2,N); askCase(L,N,X)).

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
  write('Entrez le numero de la case dans laquelle vous voulez aller:'),
  nl,
  read(Y),
  %Pour avoir le nombre de pions Ã  bouger on prend ce que renvoie le compteur dans checkMove
  checkMove(X,Y,Z,Z2),
  %Et on rajoute 1
  Z3 is Z2+1,
  normalize(Z3,Z4),
  makeMove(X,Y,Z4,L,L2).
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
numberOfMovablePions(N,[_|R],Z) :- numberOfMovablePions(N,R,Z1), !, Z is Z1+1.
%numberOfMovablePions(N,_,Z) :- numberOfMovablePions(N,[],Z).

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
% makeMove(X,Y,Z,L,L2,N) :- getListCase(X,L,L3), getListCase(Y,L,L4),
% addPionsListCase2(L3,L4,L5), removePionsListCase(L3,Z,L5)
% ,setNewList(X,Y,L,L5,L6,L2).

makeMove(X,Y,Z,L,L2) :- getListCase(X,L,ListeCaseX), getListCase(Y,L,ListeCaseY) , getPions(ListeCaseX,Z,ListePionsAdd), getListPions(ListeCaseX,ListePionsX), removePions(ListePionsX,Z,ListeCaseXRemoved), newListeCase(X,ListeCaseXRemoved,NewListeCaseX), getListPions(ListeCaseY,ListePionsY), addPions(ListePionsY,ListePionsAdd,ListePionsCaseYAdded),newListeCase(Y,ListePionsCaseYAdded,L5), setNewList(X,Y,L,NewListeCaseX,L5,L2).

newListeCase(N,L,[[N],L]).

removePions(L,0,L).
removePions([_|R],Z,L) :- Z1 is Z-1, removePions(R,Z1,L).

addPions(ListeCaseY,[],ListeCaseY).
addPions(Liste,[A|R],[A|R2]) :- addPions(Liste,R,R2).

%makeMove(X,Y,Z,L,L2) :- getListCase(X,L,L3), getListCase(Y,L,L6),
  %getListPions(L3,L4),lookforLadd(L4,Z,Ladd), flatten(Ladd,Lpions), removePionsListCase(L3,Z,L5),
  %addPions(L6,Lpions,LPions2), setNewList(X,Y,L,L5,[[Y],LPions2],L2), write(L2).
  %lookforLadd(L4,Z,Ladd), flatten(Ladd,LFin), flatten([LFin|L7],L8),
  %write(L8), setNewList(X,Y,L,L5,[[Y],L8],L2).

%lookforLadd(_,0,[]).
% lookforLadd([A|R], Z, [A|L2]) :- Z > 1, Z1 is Z-1, nl,
% lookforLadd(R,Z1,L2).
%lookforLadd([A|R], 1, A) :- lookforLadd(R,0,[]).

getPions([_,[A|_]],1,[A]).
getPions([_,[A,B|_]],2,[A,B]).
getPions([_,[A,B,C|_]],3,[A,B,C]).

%addPions([_,LP],L3,L4) :- append(L3,LP,L4).

% addPionsListCase2(ListeCaseD, ListeCaseA, Z, ListeAFin) :-
% getPions(ListeCaseD,Z,ListePions),

% -----------------------------------------------------------------------
% Predicat qui enleve Z pions d'une liste d'une case et retourne la
% liste sans les Z pions
% removePionsListCase(+L,+Z,?L2).
% L = liste de la case
% Z = nombre de pions Ã  enlever
% L2 = nouvelle liste
% -----------------------------------------------------------------------
% removePionsListCase(L,Z,L2) :- getListPions(L,L3),
% removePions(L3,Z,L4), setListCase(L,L4,L2).

% -----------------------------------------------------------------------
% Predicat qui enleve Z pions a une liste de pions
% removePions(+L,+Z,?L2).
% L = liste debut
% Z = nombre de pions
% L2 = liste pions fin
% -----------------------------------------------------------------------
%removePions(L,0,L).
%removePions([_|R], Z, L) :- Z > 0, Z1 is Z-1, removePions(R,Z1,L).

% -----------------------------------------------------------------------
% Predicat qui met a jour le nombre de pions d'une liste correspondant
% a une case
% setListCase(+L,+L4,?L2).
% L = liste case debut
% L4 = liste pions
% L2 = nouvelle liste case
% -----------------------------------------------------------------------
%setListCase([A,_],L4,[A,L4]).

% -----------------------------------------------------------------------
% Predicat qui ajoute Z pions a liste d'une case et retourne la liste
% avec les Z pions en plus
% addPionsListCase(+L,+Z,?L2,+N).
% L = liste de la case
% Z = nombre de pions Ã  ajouter
% L2 = nouvelle liste avec les pions
% N = numero du joueur
% ----------------------------------------------------------------------
% addPionsListCase(L,Z,L2,N) :- getListPions(L,L3), addPions(L3,Z,L4,N),
% setListCase(L,L4,L2).

% -----------------------------------------------------------------------
% Predicat qui ajoute Z pions a une liste de pions
% addPions(+L,+Z,?L2,+N).
% L = liste debut
% Z = nombre de pions
% L2 = liste pions fin
% N = numero du joueur
% -----------------------------------------------------------------------
%addPions(L,0,L,_).
%addPions(L, Z, [N|R], N) :- Z > 0, Z1 is Z-1, addPions(L,Z1,R,N).

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


%-----------------------------------------------------------------------
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




%minimax(_,[],[]).
%minimax(N,[L|R],[I|R2]) :- minimax_2(N,L,I,-10), minimax(N,R,R2).

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


%------------------------------------------------------------------------
%A voir
% -----------------------------------------------------------------------
%getList([L|_],I,L2) :- length(L,A), A >= I, getList2(L,I,L2).
%getList([L|R],I,L2) :- length(L,A), A < I, I1 = I-A, getList(R,I1,L2).
%getList2([L|_],0,L).
%getList2([_|R],I,L2) :- I1 is I-1, getList2(R,I1,L2).





% Predicat qui permet d'ajouter un element en premiere position d'une
% liste
%addMin(Min,L,[Min|L]).

%Predicat qui retourne le premier element d'une liste
%firstElement([A|_],A).

%Predicat qui supprime le dernier element d'une liste
%deleteFirstElement([_|R],R).

getAllMovesPlayerHeuristicsMin(N,L,LH2,Min) :- playerAdv(N,N2), getCasesPlayer(L,LC,N2), getAllMoves3(LC,L,LE,N2,Min), calculateHeuristics(N,LE,LH), flatten(LH,LH2).


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

% generateStateMin(C,C2,LJ,LJ2,N,3) :- !, playerAdv(N,N2),
% transition(C,C2,NP), makeMove(C,C2,NP,LJ,LJ2,N),
% calculateHeuristicSimple(N2,LJ2,H), H>=4.
% generateStateMin(C,C2,LJ,LJ2,N,3) :- playerAdv(N,N2),
% transition(C,C2,NP), makeMove(C,C2,NP,LJ,LJ2,N),
% calculateHeuristicSimple(N2,LJ2,H), H==3.
generateStateMin(C,C2,LJ,LJ2,N,Min) :- playerAdv(N,N2), transition(C,C2,NP), makeMove(C,C2,NP,LJ,LJ2), calculateHeuristicSimple(N2,LJ2,H), Min \= 0, H>=Min.
generateStateMin(C,C2,LJ,LJ2,N,0) :- playerAdv(N,N2), transition(C,C2,NP), makeMove(C,C2,NP,LJ,LJ2), calculateHeuristicSimple(N2,LJ2,H), H>=1.

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

vide([]).

premier([A|_],A).

player1Win(L):- getListCase(1,L,[_,M]),isControllingCase(M,A),(A\=2;vide(A)),
getListCase(2,L,[_,N]),isControllingCase(N,B),(B\=2;vide(B)),
getListCase(3,L,[_,O]),isControllingCase(O,C),(C\=2;vide(C)),
getListCase(4,L,[_,P]),isControllingCase(P,D),(D\=2;vide(D)),
getListCase(5,L,[_,Q]),isControllingCase(Q,E),(E\=2;vide(E)),
getListCase(6,L,[_,R]),isControllingCase(R,F),(F\=2;vide(F)),
getListCase(7,L,[_,S]),isControllingCase(S,G),(G\=2;vide(G)),
getListCase(8,L,[_,T]),isControllingCase(T,H),(H\=2;vide(H)),
getListCase(9,L,[_,U]),isControllingCase(U,I),(I\=2;vide(I)),
  printList(L), nl,
write('Joueur 1 GAGNE !').


player2Win(L):- getListCase(1,L,[_,M]),isControllingCase(M,A),(A\=1;vide(A)),
getListCase(2,L,[_,N]),isControllingCase(N,B),(B\=1;vide(B)),
getListCase(3,L,[_,O]),isControllingCase(O,C),(C\=1;vide(C)),
getListCase(4,L,[_,P]),isControllingCase(P,D),(D\=1;vide(D)),
getListCase(5,L,[_,Q]),isControllingCase(Q,E),(E\=1;vide(E)),
getListCase(6,L,[_,R]),isControllingCase(R,F),(F\=1;vide(F)),
getListCase(7,L,[_,S]),isControllingCase(S,G),(G\=1;vide(G)),
getListCase(8,L,[_,T]),isControllingCase(T,H),(H\=1;vide(H)),
getListCase(9,L,[_,U]),isControllingCase(U,I),(I\=1;vide(I)),
  printList(L), nl,
write('Joueur 2 GAGNE !').




%getList(LE3,R,L2).

% aiPlay(N,L,L2) :- getAllMovesPlayer(N,L,LE), my_flat_list(LE,LE2),
% minimax(N,LE2,I), flatten(I,I2), write(I2), indexMax(I2,Z,Z2),
% getList(LE,Z2,L2).

%minimax(_,[],[],_).
 %minimax_2(N,[L|R],[I|R2], Min) :-
 %(getAllMovesPlayerHeuristics(N,L,LH,Min), listMin(LH,I), ((I>Min,
 %minimax_2(N,R,R2,I));minimax_2(N,R,R2,I)); minimax_2(N,R,R2,Min)).
 %minimax(N,[L|R],[I|R2],Min) :- getAllMovesPlayerHeuristics(N,L,LH,Min),
 %!, write(LH), listMin(LH,I), write(I), nl, minimax(N,R,R2,I).
% minimax(N,[_|R],[-100|R2],Min) :- write("100"), nl,
% minimax(N,R,R2,Min).


antoine() :- getAllMovesPlayer(2,[[[1],[]],[[2],[2,1,1,1,2,2]],[[3],[]],[[4],[]],[[5],[]],[[6],[]],[[7],[]],[[8],[1,2,1,1,2]],[[9],[2]]],LE), my_flat_list(LE,LE2), write(LE2).



% -----------------------------------------------------------------------
% LEVEL 1
% ----------------------------------------------------------------------
% -----------------------------------------------------------------------
% Predicat qui permet de calculer la meilleure solution à jouer pour le
% niveau 1
% aiPlay(+N,+L,?L2,+I).
% N = numéro du joueur
% L = liste plateau de jeu
% L2 = liste après tour suivant
% -----------------------------------------------------------------------
aiPlay(N,L,L2) :- getAllMovesPlayer(N,L,LE), my_flat_list(LE,LE2),
minimax(N,LE2,I), flatten(I,I2), listMax(I2,Max),
deleteBadPlays(LE2,I2,Max,LE3), deleteBadHeuristics(I2,Max,I3), length(I3,A), random(0,A,R), nth0(R,LE3,L2).

% -----------------------------------------------------------------------
% Predicat qui realise l'algorithme min-max
% minimax(+N,+L,?I).
% N = numero du joueur
% L = liste d'etats a traiter
% I = liste des heuristiques minimums correspondant a l'etat de la liste
% L au meme index
% -----------------------------------------------------------------------
minimax(_,[],[]).
minimax(N,[L|R],[10|R2]) :- calculateHeuristicSimple(N,L,10), !, minimax(N,R,R2).
minimax(N,[L|R],[-10|R2]) :- calculateHeuristicSimple(N,L,-10), !, minimax(N,R,R2).
minimax(N,[L|R],[I|R2]) :-
  getAllMovesPlayerHeuristics(N,L,LH2), listMin(LH2,I),  minimax(N,R,R2).

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
getAllMovesPlayerHeuristics(N,L,LH2) :- playerAdv(N,N2), getCasesPlayer(L,LC,N2), getAllMoves(LC,L,LE,N2), calculateHeuristics(N,LE,LH), flatten(LH,LH2).

% Predicat qui retourne tous les etats possibles a partir de l'etat
% actuel (sans les heuristiques)
getAllMovesPlayer(N,L,LE) :-getCasesPlayer(L,LC,N), getAllMoves(LC,L,LE,N).

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
getAllMoves([],_,[],_).
getAllMoves([C|R],LP,[LE|R2],N) :- getCase(C,LC), numCase(NC,LC), getListPions(C,LP2), numberOfMovablePions(N,LP2,Z), getMovesFromCases(NC,L2,Z), flatten(L2,L), generateAllStates(NC,L,LP,LE), getAllMoves(R,LP,R2,N).

% -----------------------------------------------------------------------
% Predicat qui permet de generer tous les etats possibles d'une case
% de depart a plusieurs cases d'arrivees
% generateAllStates(C,LC,LJ,LJ2,N).
% C = case de depart
% LC = liste des cases d'arrivee
% LJ = Liste correspondant au plateau
% LJ2 = liste correspondant aux etats possibles a  partir de la case de
% depart et des cases d'arrivees
% On intègre directement alpha-beta : si min joue, si la valeur la plus
% petite n'est pas dépassée on termine les recherches pour la case de
% départ C Si max joue, si la valeur max n'est pas dépassée on termine
% les recherches pour la case de départ C
% -----------------------------------------------------------------------
generateAllStates(_,[],_,[]).
generateAllStates(C,[C2|R],LJ,[LJ2|R2]) :- generateState(C,C2,LJ,LJ2), generateAllStates(C,R,LJ,R2).

% -----------------------------------------------------------------------
% Predicat qui permet de generer l'etat possible a partir d'une case
% de depart et d'une case d'arrivee.
% generateState(+C,+C2,+LJ,?LJ2,+N).
% C = case de depart
% C2 = case arrivee
% LJ = Liste du plateau de jeu
% LJ2 = liste etat suivant
% -----------------------------------------------------------------------
generateState(C,C2,LJ,LJ2) :- transition(C,C2,NP), makeMove(C,C2,NP,LJ,LJ2).

% -----------------------------------------------------------------------
% Predicat qui supprime les heuristiques de la liste I inferieurs au Max
% deleteBadHeurisitcs(+I,+Max,+L).
% I = liste des heuristiques
% Max = valeur maximale
% L = liste des heuristiques sans les valeurs inférieurs au Max
% -----------------------------------------------------------------------
deleteBadHeuristics([],_,[]).
deleteBadHeuristics([I|R],Max,L) :- I < Max, deleteBadHeuristics(R,Max,L).
deleteBadHeuristics([I|R],Max,[I|R2]) :- I >= Max, deleteBadHeuristics(R,Max,R2).

% Meme predicat qui precedemment mais supprime des liste d'etat
deleteBadPlays([],[],_,[]).
deleteBadPlays([_|R],[I|R2],Max,LE3) :- I<Max, deleteBadPlays(R,R2,Max,LE3).
deleteBadPlays([L|R],[I|R2],Max,[L|R3]) :- I>=Max, deleteBadPlays(R,R2,Max,R3).

% ----------------------------------------------------------------------
% Predicat qui permet de faire un flatten d'une liste sur un niveau
% seulement
% my_flat_list(+L,?L2).
% L = liste avant flatten
% L2 = liste avec flatten
% -----------------------------------------------------------------------
my_flat_list([], []).
my_flat_list([A|B],L) :- is_list(A), my_flat_list(B,B1), !, append(A,B1,L).
my_flat_list([A|B],[A|B1]) :- my_flat_list(B,B1).

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

