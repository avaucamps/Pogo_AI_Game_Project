
% Test :
% [[[1],[1,1]],[[2],[1,1]],[[3],[1,1]],[[4],[]],[[5],[]],[[6],[]],[[7],[2,2]],[[8],[2,2]],[[9],[2,2]]]
%

%------------------------------------------------------------------------
% Prédicat qui fait commencer la partie en créant le plateau de jeu
% L correspond au plateau de jeu
% -----------------------------------------------------------------------
start_game() :- create_list(1,L), write(L), tourJoueur1(L).

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
% L correspond à la liste du jeu (plateau)
% X correspond à la case sur laquelle le joueur veut aller
% -----------------------------------------------------------------------
tourJoueur1(1,L,_) :- tourJoueur2(L).
tourJoueur1(0,L,X) :- read_start_move(X,L,1).

% -----------------------------------------------------------------------
% Prédicat qui permet de demander à un joueur quel mouvement il veut
% réaliser, et s'assurer que le mouvement est réalisable.
% X correspond au numéro de la case de laquelle il veut partir
% L correspond a la liste de jeu (plateau)
% N correspond au numéro du joueur
% Z2 correspond au nombre de pièces que le joueur peut bouger
% -----------------------------------------------------------------------
read_start_move(X,L,N) :-
  write('Entrez le numéro de la case de laquelle vous voulez partir:'),
  nl,
  read(X),
  %isCase(X),
  infosMove(X,L,N,Z2),
  print_nbMoves(Z2).

%------------------------------------------------------------------------
% Prédicat qui permet d'éxécuter un déplacement
% X = case de départ
% Y = case d'arrivée
% L = plateau de jeu
% Z = nombre de déplacements maximum
% -----------------------------------------------------------------------
read_end_move(X,L,Z) :-
  write('Entrez le numéro de la case dans laquelle vous voulez aller:'),
  nl,
  read(Y).
  checkMove(X,Y,L,Z).
  makeMove(X,Y,L).
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
infosMove(1,[L|_],N, Z2) :- getListPions(L,L2), isControllingCase(L2,N), numberOfMovablePions(N,L2,Z), normalize(Z,Z2).
infosMove(X,[_|R],N,Z2) :- X1 is X-1, isOwnCase(X1,R,N,Z2).

%------------------------------------------------------------------------
% Prédicat qui permet de savoir si le joueur controle la case, c'est à
% dire que le dernier pion appartient au joueur en question
% L = plateau de jeu
% N = numéro du joueur et ses pions
% -----------------------------------------------------------------------
isControllingCase(L,N) :- last(L,N).

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





