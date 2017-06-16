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
