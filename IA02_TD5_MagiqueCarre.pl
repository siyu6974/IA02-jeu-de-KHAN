% IA02-TD05-Carre Magique

% Partie I
% Q1
dimension(Caree,Dim):-longeur(Carre,Dim).
% Q2
element_n(N,L,X).
element_n(1,[T|_],T):-!.
element_n(N,[_|Q],X):-M is N-1, element_n(M,Q,X).
%
ligne_n(N,Carre,L):-element_n(N,Carre,L).

% colonne_n(N,Carre,C).
colonne_n(N,[L],[X]):-element_n(N,L,X),!.
colonne_n(N,[L1|Q],[X,Res]):-element_n(N,L1,X),colonne_n(N,Q,Res).

% colonnes(Carre,Cs)
colonnesI(Dim,Carre,[DC]):-dimension(Carre,Dim),colonne_n(Dim,Carre,DC).
colonnesI(I,Carre,[CN|Res]):-colonne_n(I,Carre,CN),J is I+1, colonne_n(J,Carre,Res).
colonnes(Carre,Cs):-colonnesI(1,Carre,Cs).

% diagonale1(Carre,D1)
diagonale_1(N,[L],[X]):-element_n(N,L,X),!.
diagonale_1(I,[L|Q],[X|D1Res]):-element_n(I,L,X),J is I+1, diagonale_1(J,Q,D1Res).
diagonale1(Carre,D1):-diagonale_1(1,Carre,D1).

diagonale_2(1,[L],[X]):-element_n(1,L,X),!.
diagonale_2(J,[J|Q],[X|D2Res]):-element_n(J,L,X),K is J-1,diagonale_2(K,Q,D2Res).
diagole2(Carre,D2):-dimension(Carre,Dim),diagonale_2(Dim,Carre,D2).

%
composantes(Carre,Comp):-
	colonnes(Carre,Cols),
	diagonale(Carre,D1),
	diagonale(Carre,D2),
	concat(Cols,[D1,D2],CD),
	concat(Carre,CD,Comp).
	
%
magique(Carre):-composantes(Carre,[T|Q]),somme(T,S),meme_somme(Q,S).
meme_somme([],_).
meme_somme([T|Q],S):-meme_somme(Q,S).
