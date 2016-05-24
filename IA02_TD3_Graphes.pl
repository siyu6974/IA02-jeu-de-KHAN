%IA02_TD3_Graphes
graphe([(1,2),(1,6),(2,3),(2,4),(3,4),(4,5)]).

succ(X,Y,G):-element((X,Y),G).

%degre(X,D,G)
degre(X,0,[]).
degre(X,D,[(X,_)|Q]):-!,degre(X,N,Q), D is N+1.
degre(X,D,[(_,X)|Q]):-!,degre(X,N,Q), D is N+1.
degre(X,D,[(_,_)|Q]):-!,degre(X,D,Q).

%sommets(E,G)
sommets([X,Y|Reste],[(X,Y)|Q]):- X\=Y,degre(X,0,Q),degre(Y,0,Q),sommets(Reste,Q),!.
sommets([X|Reste],[(X,_)|Q]):- degre(X,0,Q),sommets(Reste,Q),!.
sommets([X|Reste],[(_,X)|Q]):- degre(X,0,Q),sommets(Reste,Q),!.
sommets([],[]).

%Soit un graphe sans circuit
%chemin
chemin(X,Y,G):-succ(X,Y,G),!.
chemin(X,Y,G):-succ(X,Z,G),chemin(Z,Y,G),!.

chemin2(X,Y,G,[X,Y]):-succ(X,Y,G),!.
chemin2(X,Y,G,[X|C]):-succ(X,Z,G),chemin2(Z,Y,G,C),!.
