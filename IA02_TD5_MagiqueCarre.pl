% IA02-TD05-Carre Magique
longeur(Carre,Dim):-length(Carre,Dim).
% Partie I
% Q1
dimension(Carre,Dim):-longeur(Carre,Dim).
concat(A,B,C):-append(A,B,C).
somme(L,X):-sum_list(L,X).
% Q2
% element_n(N,L,X).
element_n(1,[T|_],T):-!.
element_n(N,[_|Q],X):-M is N-1, element_n(M,Q,X).
%
ligne_n(N,Carre,L):-element_n(N,Carre,L).

% colonne_n(N,Carre,C).
colonne_n(N,[L],[X]):-element_n(N,L,X),!.
colonne_n(N,[L1|Q],[X|Res]):-element_n(N,L1,X),colonne_n(N,Q,Res).
% colonne_n(1,[L],[X]):-element_n(1,L,X),!.
% colonne_n(1,[L1|Q],[X|Res]):-element_n(1,L1,X),colonne_n(1,Q,Res).
% colonne_n(2,[L],[X]):-element_n(2,L,X),!.
% colonne_n(2,[L1|Q],[X|Res]):-element_n(2,L1,X),colonne_n(2,Q,Res).
% colonne_n(3,[L],[X]):-element_n(3,L,X),!.
% colonne_n(3,[L1|Q],[X|Res]):-element_n(3,L1,X),colonne_n(3,Q,Res).

% colonnes(Carre,Cs):-setof(C,(N^colonne_n(N,Carre,C)),Cs). 只有在所有情况都被列举后才能这么写

% colonnes(Carre,Cs)
colonnesI(Dim,Carre,[DC]):-dimension(Carre,Dim),colonne_n(Dim,Carre,DC).
colonnesI(I,Carre,[CN|Res]):-colonne_n(I,Carre,CN),J is I+1, colonnesI(J,Carre,Res).
colonnes(Carre,Cs):-colonnesI(1,Carre,Cs),!.

% diagonale1(Carre,D1)
diagonale_1(N,[L],[X]):-element_n(N,L,X),!.
diagonale_1(I,[L|Q],[X|D1Res]):-element_n(I,L,X),J is I+1, diagonale_1(J,Q,D1Res).
diagonale1(Carre,D1):-diagonale_1(1,Carre,D1).

diagonale_2(1,[L],[X]):-element_n(1,L,X),!.
diagonale_2(J,[L|Q],[X|D2Res]):-element_n(J,L,X),K is J-1,diagonale_2(K,Q,D2Res).
diagole2(Carre,D2):-dimension(Carre,Dim),diagonale_2(Dim,Carre,D2).

%
composantes(Carre,Comp):-
	colonnes(Carre,Cols),
	diagonale1(Carre,D1),
	diagonale2(Carre,D2),
	concat(Cols,[D1,D2],CD),
	concat(Carre,CD,Comp).

%
magique(Carre):-composantes(Carre,[T|Q]),somme(T,S),meme_somme(Q,S).
meme_somme([],_).
meme_somme([T|Q],S):-somme(T,S),meme_somme(Q,S).

% Partie 2
genere_liste(1,[1]).
genere_liste(UB,[UB,L]):-NB is UB-1, genere_liste(NB,L).

retire_el([T|Q],T,Q).
retire_el([T|Q],X,[T|LRes]):-retire_el(Q,X,LRes).

genere_ligne(0,listeNbs,[],ListeNbs):-!.
genere_ligne(N,ListeNbs,[X|Reste],ListeNbsR):-retire_el(ListeNbs,X,RTemp),M is N-1, genere_ligne(M,RTemp,Reste,ListeNbsR).