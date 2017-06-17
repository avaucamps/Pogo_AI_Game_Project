% -----------------------------------------------------------------------
% Predicat qui permet d'afficher la liste representant le plateau de
% jeu sous la forme d'une matrice 3x3
% printList(+L).
% L = liste plateau de jeu
% ----------------------------------------------------------------------
printList(L):- write('-------------------------------'),nl,afficheLigne1(L),nl,write('------------'),nl,afficheLigne2(L),nl,write('------------'),nl,afficheLigne3(L),nl, write('-------------------------------'),nl.
afficheLigne1(L):- getListCase(1,L,[_,X]),write(X),write(' | '),
  getListCase(2,L,[_,Y]),write(Y),write(' | '),
  getListCase(3,L,[_,Z]),write(Z).

afficheLigne2(L):- getListCase(4,L,[_,X]),write(X),write(' | '),
  getListCase(5,L,[_,Y]),write(Y),write(' | '),
  getListCase(6,L,[_,Z]),write(Z).

afficheLigne3(L):- getListCase(7,L,[_,X]),write(X),write(' | '),
  getListCase(8,L,[_,Y]),write(Y),write(' | '),
  getListCase(9,L,[_,Z]),write(Z).

%------------------------------------------------------------------------
% Predicat qui permet de dire a l'utilisateur combien de mouvements il
% peut effectuer
% -----------------------------------------------------------------------
print_nbMoves(Z2) :- Z2 > 1, write("Vous pouvez effectuer de 1 a "),write(Z2),write(" deplacements.").
print_nbMoves(_) :- write("Vous pouvez effectuer 1 deplacement.").


play :-
    repeat,
    nl,
    writeln('***** MENU PRINCIPAL *****'),
    nl,
    writeln('Veuillez séléctionner le mode de jeu que vous souhaitez.'),
    writeln('1. Player vs Player'),
    writeln('2. Player vs IA'),
    writeln('3. IA vs IA'),
    writeln('4. Exit'),
    nl,
    writeln('***************************'),
    read(X),
    game_mode(X).

game_mode(1) :- writeln('** Numérotation des cases **'),
	writeln('-------------'),
	writeln('| 1 , 2 , 3 |'),
	writeln('| 4 , 5 , 6 |'),
	writeln('| 7 , 8 , 9 |'),
	writeln('-------------'),
	writeln('****************************'),
pogo(0).

game_mode(2) :- playervsia.

game_mode(3) :- iavsia.

game_mode(4) :-writeln('sortie').

game_mode(_) :-
    writeln('erreur de saisie'), fail.

playervsia :-
    repeat,
    nl,
    writeln('** Sélection de la difficulté **'),
    nl,
    writeln('1. Niveau 1'),
    writeln('2. Niveau 2'),
    writeln('3. Niveau 3'),
    writeln('4. Retour'),
    nl,
    writeln('*********************************'),
    read(X),
    modeia(X).

modeia(1):-writeln('** Numérotation des cases **'),
	writeln('-------------'),
	writeln('| 1 , 2 , 3 |'),
	writeln('| 4 , 5 , 6 |'),
	writeln('| 7 , 8 , 9 |'),
	writeln('-------------'),
	writeln('****************************'),
        pogo(1,1). %difficulté 1
modeia(2):-writeln('** Numérotation des cases **'),
	writeln('-------------'),
	writeln('| 1 , 2 , 3 |'),
	writeln('| 4 , 5 , 6 |'),
	writeln('| 7 , 8 , 9 |'),
	writeln('-------------'),
	writeln('****************************'),
        pogo(1,2). %difficulté 2
modeia(3):-writeln('** Numérotation des cases **'),
	writeln('-------------'),
	writeln('| 1 , 2 , 3 |'),
	writeln('| 4 , 5 , 6 |'),
	writeln('| 7 , 8 , 9 |'),
	writeln('-------------'),
	writeln('****************************'),
        pogo(1,3). %difficulté 3
modeia(4):-play.    %Retour menu principal

%choix de la difficulté de IA1

iavsia :-
    repeat,
    nl,
    writeln('******* Difficulté de IA1 *******'),
    nl,
    writeln('1. Niveau 1'),
    writeln('2. Niveau 2'),
    writeln('3. Niveau 3'),
    writeln('4. Retour'),
    nl,
    writeln('*********************************'),
    read(X),
    modeia2(X).

modeia2(1):-iavsia2(1). %difficulté 1
modeia2(2):-iavsia2(2). %difficulté 2
modeia2(3):-iavsia2(3). %difficulté 3
modeia2(4):-play.    %Retour menu principal

%choix de la difficulté de IA2

iavsia2(N1) :-
    repeat,
    nl,
    writeln('******* Difficulté de IA2 *******'),
    nl,
    writeln('1. Niveau 1'),
    writeln('2. Niveau 2'),
    writeln('3. Niveau 3'),
    writeln('4. Retour'),
    nl,
    writeln('*********************************'),
    read(X),
    pogoiaia(2,N1,X).
