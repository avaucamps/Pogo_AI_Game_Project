
% Test :
% [[[1],[1,1]],[[2],[1,1]],[[3],[1,1]],[[4],[]],[[5],[]],[[6],[]],[[7],[2,2]],[[8],[2,2]],[[9],[2,2]]]
%

%------------------------------------------------------------------------
% Pr�dicat qui fait commencer la partie en cr�ant le plateau de jeu
% L correspond au plateau de jeu
% -----------------------------------------------------------------------
start_game() :- create_list(1,L), write(L), tourJoueur1(L).

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
% L correspond � la liste du jeu (plateau)
% X correspond � la case sur laquelle le joueur veut aller
% -----------------------------------------------------------------------
tourJoueur1(1,L,_) :- tourJoueur2(L).
tourJoueur1(0,L,X) :- read_start_move(X,L,1).

% -----------------------------------------------------------------------
% Pr�dicat qui permet de demander � un joueur quel mouvement il veut
% r�aliser, et s'assurer que le mouvement est r�alisable.
% X correspond au num�ro de la case de laquelle il veut partir
% L correspond a la liste de jeu (plateau)
% N correspond au num�ro du joueur
% Z2 correspond au nombre de pi�ces que le joueur peut bouger
% -----------------------------------------------------------------------
read_start_move(X,L,N) :-
  write('Entrez le num�ro de la case de laquelle vous voulez partir:'),
  nl,
  read(X),
  %isCase(X),
  infosMove(X,L,N,Z2),
  print_nbMoves(Z2).

%------------------------------------------------------------------------
% Pr�dicat qui permet d'�x�cuter un d�placement
% X = case de d�part
% Y = case d'arriv�e
% L = plateau de jeu
% Z = nombre de d�placements maximum
% -----------------------------------------------------------------------
read_end_move(X,L,Z) :-
  write('Entrez le num�ro de la case dans laquelle vous voulez aller:'),
  nl,
  read(Y).
  checkMove(X,Y,L,Z).
  makeMove(X,Y,L).
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
infosMove(1,[L|_],N, Z2) :- getListPions(L,L2), isControllingCase(L2,N), numberOfMovablePions(N,L2,Z), normalize(Z,Z2).
infosMove(X,[_|R],N,Z2) :- X1 is X-1, isOwnCase(X1,R,N,Z2).

%------------------------------------------------------------------------
% Pr�dicat qui permet de savoir si le joueur controle la case, c'est �
% dire que le dernier pion appartient au joueur en question
% L = plateau de jeu
% N = num�ro du joueur et ses pions
% -----------------------------------------------------------------------
isControllingCase(L,N) :- last(L,N).

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





