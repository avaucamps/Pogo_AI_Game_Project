% Test :
% [[[1],[1,1]],[[2],[1,1]],[[3],[1,1]],[[4],[]],[[5],[]],[[6],[]],[[7],[2,2]],[[8],[2,2]],[[9],[2,2]]]
%

%------------------------------------------------------------------------
% Pr�dicat qui fait commencer la partie en cr�ant le plateau de jeu
% L correspond au plateau de jeu
% -----------------------------------------------------------------------
start_game(L) :- create_list(1,L), printList(L), tourJoueur1(0,L).

% -----------------------------------------------------------------------
% Pr�dicat qui permet d'afficher la liste repr�sentant le plateau de jeu
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
% Pr�dicat qui permet de cr�er la liste constituant le plateau
% On instancie les 9 cases qui composent le plateau
% N correspond au num�ro de la case, L correspond � la liste contenant
% d'une part une liste avec le num�ro, d'autre part une liste avec les
% pions presents sur cette case
% -----------------------------------------------------------------------
create_list(10,[]).
create_list(N,[L|R]) :- instancier_case(N,L), N1 is N+1, create_list(N1,R).

%------------------------------------------------------------------------
% Pr�dicat qui permet d'instancier une case avec son num�ro et sa liste
% de pions
% N correspond au num�ro de la case, L � la liste (plateau de jeu)
%-----------------------------------------------------------------------

instancier_case(N,[L1,L2]) :- numCase(N,L1), addPionsStart(N,L2).

%------------------------------------------------------------------------
% Pr�dicat qui retourne le num�ro d'une case
% -----------------------------------------------------------------------
numCase(N,[N]).

%------------------------------------------------------------------------
% Pr�dicat qui ajoute les pions sur la plateau au d�but de la partie
% N correspond au num�ro du joueur : 1 ou 2
% -----------------------------------------------------------------------
addPionsStart(N,[1,1]) :- N < 4, !.
addPionsStart(N,[2,2]) :- N > 6, !.
addPionsStart(_,[]).

%------------------------------------------------------------------------
% Pr�dicat qui permet au joueur 1 de r�aliser son tour
% Une fois que son tour est fini c'est au tour de l'autre joueur
% Premier param�tre = tour jou� (1) ou pas (0)
% L correspond � la liste du jeu (plateau)
% X correspond � la case sur laquelle le joueur veut aller
% -----------------------------------------------------------------------
tourJoueur1(1,L) :- printList(L), tourJoueur2(0,L).
tourJoueur1(0,L) :- read_start_move(X,L,1,Z), read_end_move(X,L,Z,1,L2), tourJoueur1(1,L2).

tourJoueur2(1,L) :- printList(L), tourJoueur1(0,L).
tourJoueur2(0,L) :- read_start_move(X,L,2,Z), read_end_move(X,L,Z,2,L2), tourJoueur2(1,L2).

% -----------------------------------------------------------------------
% Pr�dicat qui permet de demander � un joueur quel mouvement il veut
% r�aliser, et s'assurer que le mouvement est r�alisable.
% X correspond au num�ro de la case de laquelle il veut partir
% L correspond a la liste de jeu (plateau)
% N correspond au num�ro du joueur
% Z2 correspond au nombre de pi�ces que le joueur peut bouger
% -----------------------------------------------------------------------
read_start_move(X,L,N,Z2) :-
  %isCase(X),
  askCase(L,N,X),
  infosMove(X,L,N,Z2),
  print_nbMoves(Z2).

% -----------------------------------------------------------------------
% Pr�dicat qui demande � un joueur de quelle case il veut partir, et
% v�rifie que cela est possible.
% L = plateau de jeu
% N = num�ro du joueur
% X = case de d�part
% -----------------------------------------------------------------------
askCase(L,N,X):-
  write('Entrez le num�ro de la case de laquelle vous voulez partir:'),
  nl,
  read(X),
  getListCase(X,L,L3),
  getListPions(L3,L2),
  isControllingCase(L2,N); askCase(L,N,X).

%------------------------------------------------------------------------
% Pr�dicat qui permet d'�x�cuter un d�placement
% X = case de d�part
% Y = case d'arriv�e
% L = plateau de jeu au d�but du tour
% Z = nombre de d�placements maximum
% Z2 = nombre de d�placements effectu�s
% L2 = plateau de jeu � la fin du tour
% -----------------------------------------------------------------------
read_end_move(X,L,Z,N,L2) :-
  write('Entrez le num�ro de la case dans laquelle vous voulez aller:'),
  nl,
  read(Y),
  %Pour avoir le nombre de pions � bouger on prend ce que renvoie le compteur dans checkMove
  checkMove(X,Y,Z,Z2),
  %Et on rajoute 1
  Z3 is Z2+1,
  normalize(Z3,Z4),
  makeMove(X,Y,Z4,L,L2,N).
  %isCase(X).

%------------------------------------------------------------------------
% Pr�dicat qui permet si le nombre pass� en param�tre est un num�ro de
% case valide
% -----------------------------------------------------------------------
isCase(X) :- numlist(1,9,L), member(X,L).

%------------------------------------------------------------------------
% Pr�dicat qui permet de savoir si le mouvement envisag� par un joueur
% est correct et donne des infos sur le futur d�placement
% X = case de laquelle le joueur veut partir
% L = plateau du jeu
% N = num�ro du joueur
% Z2 = nombre de pions que le joueur peut bouger
% -----------------------------------------------------------------------
infosMove(1,[L|_],N, Z2) :- getListPions(L,L2), numberOfMovablePions(N,L2,Z), normalize(Z,Z2).
infosMove(X,[_|R],N,Z2) :- X1 is X-1, infosMove(X1,R,N,Z2).

%------------------------------------------------------------------------
% Pr�dicat qui permet de savoir si le joueur controle la case, c'est �
% dire que le dernier pion appartient au joueur en question
% L = plateau de jeu
% N = num�ro du joueur et ses pions
% -----------------------------------------------------------------------
<<<<<<< HEAD
isControllingCase([N|_],N).

ownsCase([N|_],N).

=======
isControllingCase([],0).
isControllingCase([N|_],N).

>>>>>>> 3df79cf24d68021484460535c457bc3fca8bbd79

%------------------------------------------------------------------------
% Pr�dicat qui permet de r�cup�rer le nombre de pions que l'on peut
% d�placer
% N = num�ro du joueur et ses pions
% Z = nombre de pions d�pla�ables
% -----------------------------------------------------------------------
numberOfMovablePions(_,[],0).
numberOfMovablePions(N,[N|R],Z) :- numberOfMovablePions(N,R,Z1), !, Z is Z1+1.
numberOfMovablePions(N,_,Z) :- numberOfMovablePions(N,[],Z).

%------------------------------------------------------------------------
% Pr�dicat qui permet de r�cup�rer les pions d'une case � partir de la
% liste correspondant � la case
% -----------------------------------------------------------------------
getListPions([_,B],B).

% -----------------------------------------------------------------------
% Pr�dicat qui permet de r�cup�rer la liste d'une case, c'est a dire son
% num�ro et ses pions
% N = numero de la case
% X = liste de la case
% L = plateau de jeu
% -----------------------------------------------------------------------
getListCase(1,[X|_],X).
getListCase(N,[_|R],X) :- N>1 , N1 is N-1 , getListCase(N1,R,X).

% -----------------------------------------------------------------------
% Pr�dicat qui renvoie la liste contenant juste le num�ro de la case
% -----------------------------------------------------------------------
getCase([A,_],A).

%------------------------------------------------------------------------
% Pr�dicat qui permet de mettre le nombre maximum de pions d�pla�ables �
% trois
% -----------------------------------------------------------------------
normalize(Z,Z) :- Z < 3.
normalize(_,3).

%------------------------------------------------------------------------
% Pr�dicat qui permet de dire � l'utilisateur combien de mouvements il
% peut effectuer
% -----------------------------------------------------------------------
print_nbMoves(Z2) :- Z2 > 1, write("Vous pouvez effectuer de 1 a "),write(Z2),write(" deplacements.").
print_nbMoves(_) :- write("Vous pouvez effectuer 1 deplacement.").

% -----------------------------------------------------------------------
% Pr�dicat qui permet de conna�tre les voisins directs de chaque case
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
% Pr�dicat qui permet de v�rifier si un mouvement est possible
% X = case d�part
% Y = case arriv�e
% Z = nombre de d�placement de 1 case possibles
% Z2 = compteur de d�placements dont la valeur est le nombre de
% d�placements -1
% -----------------------------------------------------------------------
checkMove(X,Y,_,0) :- isNeighbor(X,L),member(Y,L).
checkMove(X,Y,Z,Z2) :- Z>0, Z1 is Z-1, isNeighbor(X,L),member(A,L),checkMove(A,Y,Z1,Z3), Z2 is Z3+1.
%checkMove(_,_,Z,_):- Z==0, write('C cassee').

% -----------------------------------------------------------------------
% Pr�dicat qui permet d'�x�cuter un d�placement
% makeMove(+X,+Y,+Z,+L,?L2).
% X = case d�part
% Y = case arriv�e
% Z = nombre de pions � bouger
% L = plateau de jeu avant tour
% L2 = plateau de jeu apr�s tour
% N = num�ro du joueur
% -----------------------------------------------------------------------
makeMove(X,Y,Z,L,L2,N) :- getListCase(X,L,L3), getListCase(Y,L,L4), removePionsListCase(L3,Z,L5), addPionsListCase(L4,Z,L6,N), setNewList(X,Y,L,L5,L6,L2).

% -----------------------------------------------------------------------
% Pr�dicat qui enl�ve Z pions d'une liste d'une case et retourne la
% liste sans les Z pions
% L = liste de la case
% Z = nombre de pions � enlever
% L2 = nouvelle liste
% -----------------------------------------------------------------------
removePionsListCase(L,Z,L2) :- getListPions(L,L3), removePions(L3,Z,L4), setListCase(L,L4,L2).

% -----------------------------------------------------------------------
% Pr�dicat qui enl�ve Z pions � une liste de pions L = liste d�but Z =
% nombre de pions L2 = liste pions fin
% -----------------------------------------------------------------------
removePions(L,0,L).
removePions([_|R], Z, L) :- Z > 0, Z1 is Z-1, removePions(R,Z1,L).

% -----------------------------------------------------------------------
% Pr�dicat qui met � jour le nombre de pions d'une liste correspondant �
% une case L = liste case d�but
% L4 = liste pions
% L2 = nouvelle liste case
% -----------------------------------------------------------------------
setListCase([A,_],L4,[A,L4]).

% -----------------------------------------------------------------------
% Pr�dicat qui ajoute Z pions � liste d'une case et retourne la liste
% avec les Z pions en plus
% L = liste de la case
% Z = nombre de pions � ajouter
% L2 = nouvelle liste avec les pions
% N = num�ro du joueur
% ----------------------------------------------------------------------
addPionsListCase(L,Z,L2,N) :- getListPions(L,L3), addPions(L3,Z,L4,N), setListCase(L,L4,L2).

% -----------------------------------------------------------------------
% Pr�dicat qui ajoute Z pions � une liste de pions
% L = liste d�but
% Z = nombre de pions
% L2 = liste pions fin
% N = numero du joueur
% -----------------------------------------------------------------------
addPions(L,0,L,_).
addPions(L, Z, [N|R], N) :- Z > 0, Z1 is Z-1, addPions(L,Z1,R,N).

% -----------------------------------------------------------------------
% Pr�dicat qui permet de reconstruire le plateau de jeu apr�s un tour en
% prenant compte des changements
% X = case d�part
% Y = case arriv�e
% L = plateau de jeu avant tour
% L2 = liste case d�part
% L3 = liste case arriv�e
% L4 = liste apr�s le tour
% -----------------------------------------------------------------------
setNewList(0,0,L,[],[],L).
setNewList(X,Y,[A|R],L2,L3,[L2|R2]) :- getCase(A,LC), numCase(X,LC),!, setNewList(0,Y,R,[],L3,R2).
setNewList(X,Y,[A|R],L2,L3,[L3|R2]) :- getCase(A,LC), numCase(Y,LC),!, setNewList(X,0,R,L2,[],R2).
setNewList(X,Y,[A|R],L2,L3,[A|R2]) :- setNewList(X,Y,R,L2,L3,R2).
<<<<<<< HEAD
=======
%setNewList(1,Y,[_|R],L2,L3,[L2|R2]) :- setNewList(0,Y,R,L2,L3,R2).
%setNewList(X,1,[_|R],L2,L3,[L3|R2]) :- setNewList(X,0,R,L2,L3,R2).
%setNewList(X,Y,[A|R],L2,L3,[A|R2]) :- X > 1, X1 is X-1, Y > 0, Y1 is Y
% -1, setNewList(X1,Y1,R,L2,L3,R2). setNewList(-1,Y,[_|R],L2,L3,[L2|R2])
% :- setNewList(-1,Y,R,L2,L3,R2). setNewList(X,-1,[_|R],L2,L3,[L3|R2]) :-
% setNewList(X,-1,R,L2,L3,R2).
>>>>>>> 3df79cf24d68021484460535c457bc3fca8bbd79

% -----------------------------------------------------------------------
% Pr�dicat qui compte le nombre de piles contr�l�es par un joueur pass�
% en param nbPilesPlayer(+N,+L,?X).
% N = num�ro du joueur L = plateau de jeu
% X = nombre de piles
% -----------------------------------------------------------------------
nbPilesPlayer(_,[],0).
nbPilesPlayer(N,[L|R],X) :- getListPions(L,L2), colorLastPion(L2,N), !, nbPilesPlayer(N,R,X1), X is X1+1.
nbPilesPlayer(N,[_|R],X) :- nbPilesPlayer(N,R,X).

% ------------------------------------------------------------------------
% Pr�dicat qui permet de conna�tre la couleur du pion sur le dessus
% d'une pile
% colorLastPion(+L,?C).
% L = liste des pions
% C = 1 pour couleur joueur 1, 2 pour couleur joueur 2, 0 si liste vide
% -----------------------------------------------------------------------
colorLastPion([],0).
colorLastPion([X|_],X).

% -----------------------------------------------------------------------
% Pr�dicat qui calcul nbCasesIA - nbCasesPlayer pour savoir quel coup
% faire
% Renvoie 10 si l'IA a gagn�
% diffCasesPlayer2(+L,?X).
% L = plateau de jeu
% X = nbCasesIA - nbCasesPlayer
% -----------------------------------------------------------------------
diffCasesPlayer2([],_).
diffCasesPlayer2(L,10) :- nbPilesPlayer(1,L,X), X == 0, !.
diffCasesPlayer2(L,X) :- nbPilesPlayer(1,L,Y), nbPilesPlayer(2,L,Z), X is Z-Y.

% -----------------------------------------------------------------------
<<<<<<< HEAD
% Pr�dicat qui permet de r�cup�rer tous les �tats du plateau possibles
% apr�s le coup qui va intervenir
% getAllMovesPlayer(+N,+L,?LE).
% N = num�ro du joueur
% L = plateau de jeu
% LE = liste de tous les �tats possibles
% -----------------------------------------------------------------------
getAllMovesPlayer(N,L,LE) :-getCasesPlayer(L,LC,N), getAllMoves(LC,L,LE,N), write(LE).

% -----------------------------------------------------------------------
% Pr�dicat qui permet de r�cup�rer la liste des cases contr�l�es par le
% joueur N.
% getCasesPlayer(+L,?L2,+N).
% L = plateau de jeu
% L2 = liste des cases contr�l�es par le joueur N (case = liste
% comprenant liste num case + liste pions case)
% N = num�ro du joueur
% -----------------------------------------------------------------------
getCasesPlayer([],[],_).
getCasesPlayer([C|R],[C|R2],N) :- getListPions(C,LP), ownsCase(LP,N),getCasesPlayer(R,R2,N).
getCasesPlayer([C|R],L2,N) :- getListPions(C,LP), not(ownsCase(LP,N)), getCasesPlayer(R,L2,N).
=======
% Pr�dicat qui g�n�re tous les d�placements possibles � partir de la
% liste des cases pass�e en param�tre pour un joueur donn�
% getAllMoves(+N,+L,?L2).
% N = num�ro du joueur
% L = liste des cases contr�l�es par le joueur
% L2 = liste �tat final
% -----------------------------------------------------------------------
getAllMoves(_,[],[]).
getAllMoves(N,[C|R],[M|R2]) :- getMovesCases(N,C,M), getAllMoves(N,R,R2).

% -----------------------------------------------------------------------
% Pr�dicat qui retourne une liste contenant la liste de chaque case
% contr�l�e par le joueur N
% getAllCasesPlayer(+N,+L,?L2).
% N = num�ro du joueur
% L = plateau du jeu
% L2 = liste cases joueur
% -----------------------------------------------------------------------
getAllCasesPlayer(_,[],[]).
getAllCasesPlayer(N,[L|R],[L|R2]) :- getListPions(L,L2), colorLastPion(L2,N), !, getAllCasesPlayer(N,R,R2).
getAllCasesPlayer(N,[_|R],L2) :- getAllCasesPlayer(N,R,L2).

% -----------------------------------------------------------------------
% Pr�dicat qui permet de conna�tre la liste des mouvements possibles �
% partir d'une case pass�e en param�tre
% getMovesCase(+N,+C,?M).
% N = num�ro du joueur
% C = liste case contenant le num�ro de la case et la liste des pions
% qu'elle contien
% M = cases arriv�e possible
% -----------------------------------------------------------------------
getMovesCases(N,C,C3) :- getCase(C,C2), getListPions(C,P), numberOfMovablePions(N,P,X), normalize(X,X2), getMovesFromCase(C2,C3,X2).

% -----------------------------------------------------------------------
% Pr�dicat qui retourne toutes les cases d'arriv�e possible � partir
% d'une case de d�part
% getMovesFromCase(+C,?C2,+X).
% C = case d�part
% C2 = case arriv�e
% X = nombre de d�placements max possibles
% -----------------------------------------------------------------------
getMovesFromCase(C,C2,X) :- X>0, isNeighbor(C,L), member(C2,L).
getMovesFromCase(C,C2,X) :- X>0, X1 is X-1, isNeighbor(C,L),member(A,L),getMovesFromCase(A,C2,X1).








% -----------------------------------------------------------------------
% Pr�dicat qui permet de cr�er une liste contenant tous les d�placements
% possibles � partir de la liste des cases que contr�le le joueur N
% getListMoves(+N,+L,?L2,+I).
% N = num�ro du joueur
% L = liste des cases contr�l�es par le joueur
% L2 = liste des d�placements possibles
% I = num�ro de la case d'arriv�e
% Principe : pour chaque num�ro de case on cherche pour chaque autre
% case si le d�placement est possible
% ----------------------------------------------------------------------
getListMoves(_,_,_,10).

% Si le num de la case d'arriv�e n'est pas �gale au num de la case
% d'arriv�e et que le mouvement est correct, alors en enregistre le
% d�placement et on refait le test pour la case d'arriv�e suivante
getListMoves(N,[C|LP],[L2|R],I) :- write("  "), numCase(NC,C), not(NC == I), getListPions([C|LP],LP2), numberOfMovablePions(N,LP2,Z), write(NC), write(I), write(Z), isMoveCorrect(NC,I,Z), write(" "), write(I), buildList(NC,I,L2), I1 is I+1, getListMoves(N,[C|LP],[L2|R],I1).

% Si la case d'arriv�e est �gale � la case de d�part on test le
% mouvement pour la case d'arriv�e suivante
getListMoves(N,[C|LP],L2,I) :- numCase(NC,C), NC == I, I1 is I+1, getListMoves(N,[C|LP],L2,I1).

%Si la case d'arriv�e n'est pas �gale au num de la case
% d'arriv�e et que le mouvement n'est pas correct, on refait le test pour la case d'arriv�e suivante
getListMoves(N,[C|LP],L2,I) :- numCase(NC,C), not(NC == I), getListPions([C|LP],LP2), numberOfMovablePions(N,LP2,Z), not(isMoveCorrect(NC,I,Z)), I1 is I+1, getListMoves(N,[C|LP],L2,I1).

buildList(C,I,[C,I]).


isMoveCorrect(X,Y,_) :- isNeighbor(X,L),member(Y,L).
isMoveCorrect(X,Y,Z) :- Z>0, Z1 is Z-1, isNeighbor(X,L),member(A,L),isMoveCorrect(A,Y,Z1).






>>>>>>> 3df79cf24d68021484460535c457bc3fca8bbd79

% ------------------------------------------------------------------------
% Pr�dicat qui permet d'avoir tous les �tats possibles apr�s le prochain
% tour � partir de la liste des cases contr�l�es par le joueur.
% getAllMoves(+LC,+LP,?LE,+N).
% LC = liste des cases contr�l�es par le joueur
% LP = liste correspondant au plateau
% LE = liste des �tats possibles
% N = num�ro du joueur
% -----------------------------------------------------------------------
getAllMoves([],_,_,_).
getAllMoves([C|R],LP,[LE|R],N) :- getCase(C,LC), numCase(NC,LC), getMovesFromCases(NC,L2,N), flatten(L2,L), generateAllStates(NC,L,LP,LE,N), getAllMoves(R,LP,R,N).

% ------------------------------------------------------------------------
% Pr�dicat qui permet de g�n�rer tous les �tats possibles d'une case de
% d�part � plusieurs cases d'arriv�es
% generateAllStates(C,LC,LJ,LJ2,N).
% C = case de d�part
% LC = liste des cases d'arriv�e
% LJ = Liste correspondant au plateau
% LJ2 = liste correspondant aux �tats possibles � partir de la case de
% d�part et des cases d'arriv�es
% N = num�ro du joueur
% -----------------------------------------------------------------------
generateAllStates(_,[],_,_,_).
generateAllStates(C,[C2|R],LJ,[LJ2|R2],N) :- generateState(C,C2,LJ,LJ2,N), write(LJ2), generateAllStates(C,R,LJ,R2,N).

% ------------------------------------------------------------------------
% Pr�dicat qui permet de g�n�rer l'�tat possible � partir d'une case
% de d�part et d'une case d'arriv�e.
% generateState(+C,+C2,+LJ,?LJ2,+N).
% C = case de d�part
% C2 = case arriv�e
% LJ = Liste du plateau de jeu
% LJ2 = liste �tat suivant
% N = num�ro du joueur
% -----------------------------------------------------------------------
generateState(C,C2,LJ,LJ2,N) :- transition(C,C2,NP), makeMove(C,C2,NP,LJ,LJ2,N).

% -----------------------------------------------------------------------
% Pr�dicat qui permet de savoir sur quelles cases on peut aller en
% partant d'une case avec un certain nombre de d�placements N
% getMovesFromCases(+X,?L,+N).
% X = case de d�part
% L = liste des cases d'arriv�e
% N = nombre de d�placements
% -----------------------------------------------------------------------
getMovesFromCases(X,[L|R],N) :- N>1, findall(Y,transition(X,Y,N),L), N1 is N-1, getMovesFromCases(X,R,N1).
getMovesFromCases(X,[L],1) :- findall(Y,transition(X,Y,1),L).

% -----------------------------------------------------------------------
% Tous les d�placements possibles
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

<<<<<<< HEAD
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
=======
player1Gagne(L):- getListCase(1,L,[_,X]),isControllingCase(X,A),A\=2,!,
getListCase(2,L,[_,X]),isControllingCase(X,B),B\=2,!,
getListCase(3,L,[_,X]),isControllingCase(X,C),C\=2,!,
  write('test'),write(X),
getListCase(4,L,[_,X]), write(X),isControllingCase(X,D),D\=2,!,
getListCase(5,L,[_,X]),isControllingCase(X,E),E\=2,!,
getListCase(6,L,[_,X]),isControllingCase(X,F),F\=2,!,
getListCase(7,L,[_,X]),isControllingCase(X,G),G\=2,!,
getListCase(8,L,[_,X]),isControllingCase(X,H),H\=2,!,
getListCase(9,L,[_,X]),isControllingCase(X,I),I\=2,!,
write('Joueur 1 GAGNE !').
>>>>>>> 3df79cf24d68021484460535c457bc3fca8bbd79

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
