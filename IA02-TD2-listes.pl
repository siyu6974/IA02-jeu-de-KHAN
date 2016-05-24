%IA02_TD2_Listes

tete(T,[T|Q]).
queue(Q,[_|Q]).
vide([],[]).
imprime([]).
imprime([T|Q]):-write(T),write(' '),imprime(Q).

element(X,[X|_]):-!.
element(X,[_|Q]):-element(X,Q).

dernier(X,[X]).
dernier(X,[_|Q]):-dernier(X,Q).

longeur(0,[]).
longeur(Long,[_|Q]):- longeur(L,Q), Long is L+1.

%nombre(X,L,NB)
nombre(_,[],0).
nombre(X,[X|Q],NB):-nombre(X,Q,N),Nb is N+1.
nombre(X,[_|Q],NB):-nombre(X,Q,NB).

concat([],L2,L2).
concat([T|Q],L2,[T|L]):-concat(Q,L2,L).

inverse([R],[R]).
inverse([T|Q],Res):-inverse(Q,L),concat(L,[T],Res).

% partition(X,L,L1,L2)
partition(_,[],[],[]).
partition(X,[Y|Q],[Y|Linf],Lsup):-Y=<X,partition(X,Q,Linf,Lsup).
partition(X,[Y|Q],Linf,[Y|Lsup]):-Y>X,partition(X,Q,Linf,Lsup).

% tri(L,R)
tri([],[]).
tri([T|Q],Res):-partition(T,Q,Linf,Lsup),tri(Linf,LinfRes),tri(Lsup,LsupRes),concat(LinfRes,[T|LsupRes],Res).

%sous_liste(L1,L2)
sous_liste([],_).
sous_liste([X|Q],[X|R]):-sous_liste(Q,R).
sous_liste([X|Q],[_|R]):-sous_liste([X|Q],R).

%retire_element(X,L,R)
retire_element(X,[],[]).
retire_element(X,[X|Q],Q):-!.
retire_element(X,[T|Q],[T|R]):-retire_element(X,Q,R).

%retire_elements(X,L,R)
retire_elements(_,[],[]).
retire_elements(X,[X|Q],Res):-retire_elements(X,Q,Res).
retire_elements(X,[Y|Q],[Y|Res]):- X\=Y, retire_elements(X,Q,Res).

%retire_doublons(L,E)
retire_doublons([],[]).
retire_doublons([X|Q],[X|Res]):- retire_elements(X,Q,R),retire_doublons(R,Res).

%union(E1,E2,E)
union([],L,L).
union([X|Q],L,Res):-element(X,L),union(Q,L,Res).
union([X|Q],L,[X|Res]):- \+element(X,L),union(Q,L,Res).

%intersection(E1,E2,E)
intersection([],L,[]).
intersection([X|Q],L,[X|Res]):-element(X,L),intersection(Q,L,Res).
intersection([X|Q],L,Res):- \+element(X,L),intersection(Q,L,Res).

habite(vir,amiens).
habite(del,lille).
non(P):-P,!,fail.
non(_).

main():-non(habite(del,X)).
boucleMenu:-repeat, menu,!.
menu:-read(Choix),appel(Choix),Choix=4,nl,nl,nl.
appel(1):-write('1111111'),!.
appel(4):-write('444'),!.
