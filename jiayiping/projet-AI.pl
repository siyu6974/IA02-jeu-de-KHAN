start:-write('saisir ''startpvp'' ou ''startpve''').

startpvp:-
valeur_depart,plateau_depart,pile_depart,startreserve,
/*initialiser les tableaux*/ 
findall([A,B,C],(march(M),nth(A,M,[B|X]),length(X,C)),F),write('marchandises:'),nl,affiche_plateau(F),pile(Pile),write('pile:'),write(Pile),nl,
/*affichage des tableau*/
looppvp(j1).
/*commencer à jeu humain */

startpve:-
valeur_depart,plateau_depart,pile_depart,startreserve,
findall([A,B,C],(march(M),nth(A,M,[B|X]),length(X,C)),F),write('marchandises:'),nl,affiche_plateau(F),pile(Pile),write('pile:'),write(Pile),nl,
looppve(j1).
/*c'est la meme chose que looppvp mais on commencera le boucle IA

count(A,B,C) est un prédicat pour compter il y a combien d'element A dans la list B */
count(_,[],0):-!.
count(Member,[Head|Tail],Count):- Member=Head,count(Member,Tail,TailCount),Count is TailCount + 1 ,!.
count(Member,[Head|Tail],Count):- \+Member=Head,count(Member,Tail,Count),!.

/*affiche_plateau aide à afficher une tableau*/
affiche_plateau([A|B]):- \+B=[],write(A),nl,affiche_plateau(B).
affiche_plateau([A|[]]):-write(A),nl,!.

/*ces prédicat aide à initialiser les tableau et les insere comme faits */
valeur_depart:-retractall(vl(_,_)),assertz(vl(ble,7)),assertz(vl(riz,6)),assertz(vl(cacao,6)),assertz(vl(cafe,6)),assertz(vl(sucre,6)),assertz(vl(mais,6)).
pile_depart:-retractall(pile(_)),assertz(pile(1)).
plateau_depart:-retractall(march(_)),randomize,rnd_list([ble,ble,ble,ble,ble,ble,riz,riz,riz,riz,riz,riz,cacao,cacao,cacao,cacao,cacao,cacao,cafe,cafe,cafe,cafe,cafe,cafe,mais,mais,mais,mais,mais,mais,sucre,sucre,sucre,sucre,sucre,sucre],36,[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18,A19,A20,A21,A22,A23,A24,A25,A26,A27,A28,A29,A30,A31,A32,A33,A34,A35,A36]),assertz(march([[A1,A2,A3,A4],[A5,A6,A7,A8],[A9,A10,A11,A12],[A13,A14,A15,A16],[A17,A18,A19,A20],[A21,A22,A23,A24],[A25,A26,A27,A28],[A29,A30,A31,A32],[A33,A34,A35,A36]])),!.
startreserve:-retractall(res(_,_)),assertz(res(j1,[])),assertz(res(j2,[])).

/*replace_at(Member,List,Number,List) est un prédicat pour remplacer le neme element de la liste par un membre */
replace_at(Z,[_|Xs],1,[Z|Xs]):-!.
replace_at(X,[Y|Xs],K,[Y|Ys]) :- K > 1,T is (K - 1),replace_at(X,Xs,T,Ys).

/*c'est un prédicat qui supprime la neme element dans une liste*/
remove_at(X,[X|Xs],1,Xs):-!.
remove_at(X,[Y|Xs],K,[Y|Ys]) :- K > 1, T is (K - 1),remove_at(X,Xs,T,Ys).

/*ce prédicat produit une liste avec la permutation aleatoire*/
rnd_list(_,0,[]).
rnd_list(Xs,N,[X|Zs]) :- N > 0,length(Xs,Beibei),L is Beibei+1,random(1,L,I),remove_at(X,Xs,I,Ys),N1 is N - 1,rnd_list(Ys,N1,Zs).

/*ce prédicat va enlever les element vide dans la liste marchandises et la position de la pile va changer si necessaire */
checkvide:-march(A),pile(X),nth(B,A,[]),remove_at([],A,B,Y),X>B,X1 is X-1,retract(pile(_)),assertz(pile(X1)),retract(march(A)),assertz(march(Y)),!.
checkvide:-march(A),pile(X),nth(B,A,[]),remove_at([],A,B,Y),\+(X>B),retract(march(A)),assertz(march(Y)),!.
checkvide:-march(A),\+nth(_,A,[]).

checkvide(M1,P1,M2,P2):-nth(B,M1,[]),remove_at([],M1,B,M2),P1>B,P2 is P1-1,!.
checkvide(M1,P1,M2,P1):-nth(B,M1,[]),remove_at([],M1,B,M2),\+(P1>B),!.
checkvide(M1,P1,M1,P1):- \+nth(_,M1,[]),!.
/*reserve(Joueur,Marchandise) va ajouter le marchandises à la reserve du joueur*/
reserve(X,Y):-member(X,[j1,j2]),retract(res(X,Z)),assertz(res(X,[Y|Z])).

/*ces predicats aide la deplacement de la pile*/
dep(L,A,X,N):-Y is A+X,length(L,Z),(Y>Z)->(N is Y-Z),!.
dep(L,A,X,N):-Y is A+X,length(L,Z),(Y=<Z)->(N is Y),!.
deplace(A):-member(A,[1,2,3]),pile(C),march(L),dep(L,C,A,B),retract(pile(_)),assertz(pile(B)).

/*possible_coup va tester si le coup est possible ou pas*/
possible_coup(M1,R11,R21,Vl1,P1,j1,Y,Z1,Z2,M4,R12,R21,Vl2,P4,Dif):-member(Y,[1,2,3]),Y1 is Y+1,Y2 is Y-1,dep(M1,P1,Y1,A1),dep(M1,P1,Y2,A2),(nth(A2,M1,[Z1|R1]),nth(A1,M1,[Z2|R2])),dep(M1,P1,Y,P2),R12=[Z1|R11],replace_at(R1,M1,A2,B1),replace_at(R2,B1,A1,M2),nth(Nb,Vl1,[Z2,V]),V1 is V-1,replace_at([Z2,V1],Vl1,Nb,Vl2),p(Vl2,R12,Point1),p(Vl2,R21,Point2),Dif is Point2-Point1,checkvide(M2,P2,M3,P3),checkvide(M3,P3,M4,P4).
possible_coup(M1,R11,R21,Vl1,P1,j2,Y,Z1,Z2,M4,R11,R22,Vl2,P4,Dif):-member(Y,[1,2,3]),Y1 is Y+1,Y2 is Y-1,dep(M1,P1,Y1,A1),dep(M1,P1,Y2,A2),(nth(A2,M1,[Z1|R1]),nth(A1,M1,[Z2|R2])),dep(M1,P1,Y,P2),R22=[Z1|R21],replace_at(R1,M1,A2,B1),replace_at(R2,B1,A1,M2),nth(Nb,Vl1,[Z2,V]),V1 is V-1,replace_at([Z2,V1],Vl1,Nb,Vl2),p(Vl2,R11,Point1),p(Vl2,R22,Point2),Dif is Point2-Point1,checkvide(M2,P2,M3,P3),checkvide(M3,P3,M4,P4).
possible_coup(M1,R11,R21,Vl1,P1,j1,Y,Z1,Z2,M4,R12,R21,Vl2,P4,Dif):-member(Y,[1,2,3]),Y1 is Y+1,Y2 is Y-1,dep(M1,P1,Y1,A1),dep(M1,P1,Y2,A2),(nth(A1,M1,[Z1|R1]),nth(A2,M1,[Z2|R2])),dep(M1,P1,Y,P2),R12=[Z1|R11],replace_at(R1,M1,A1,B1),replace_at(R2,B1,A2,M2),nth(Nb,Vl1,[Z2,V]),V1 is V-1,replace_at([Z2,V1],Vl1,Nb,Vl2),p(Vl2,R12,Point1),p(Vl2,R21,Point2),Dif is Point2-Point1,checkvide(M2,P2,M3,P3),checkvide(M3,P3,M4,P4).
possible_coup(M1,R11,R21,Vl1,P1,j2,Y,Z1,Z2,M4,R11,R22,Vl2,P4,Dif):-member(Y,[1,2,3]),Y1 is Y+1,Y2 is Y-1,dep(M1,P1,Y1,A1),dep(M1,P1,Y2,A2),(nth(A1,M1,[Z1|R1]),nth(A2,M1,[Z2|R2])),dep(M1,P1,Y,P2),R22=[Z1|R21],replace_at(R1,M1,A1,B1),replace_at(R2,B1,A2,M2),nth(Nb,Vl1,[Z2,V]),V1 is V-1,replace_at([Z2,V1],Vl1,Nb,Vl2),p(Vl2,R11,Point1),p(Vl2,R22,Point2),Dif is Point2-Point1,checkvide(M2,P2,M3,P3),checkvide(M3,P3,M4,P4).

/*ce prédicat va jouer le coup et refaire les faits à nouveau*/
coup(X,Y,Z1,Z2):-
member(X,[j1,j2]),member(Y,[1,2,3]),march(B),Y1 is Y+1,Y2 is Y-1,pile(Pile),
dep(B,Pile,Y1,A1),dep(B,Pile,Y2,A2),(nth(A1,B,[Z1|R1]),nth(A2,B,[Z2|R2])),
/*le coup est possible*/
deplace(Y),reserve(X,Z1),
/*changer la position du trater et la reserve du joueur*/
replace_at(R1,B,A1,B1),replace_at(R2,B1,A2,B2),
/*supprime l'ancienne liste et calculer le nouveau tableau des marchandises*/
retract(march(_)),assertz(march(B2)),
/*ajouter la nouvelle liste comme une fait*/
retract(vl(Z2,V)),V1 is V-1,assertz(vl(Z2,V1)),
/*renouveller la valeur du marchandise*/
checkvide,checkvide,!.
/*supprimer les elements vides dans la liste des marchandises*/
coup(X,Y,Z1,Z2):-member(X,[j1,j2]),member(Y,[1,2,3]),march(B),Y1 is Y+1,Y2 is Y-1,pile(Pile),dep(B,Pile,Y1,A1),dep(B,Pile,Y2,A2),(nth(A2,B,[Z1|R1]),nth(A1,B,[Z2|R2])),deplace(Y),reserve(X,Z1),replace_at(R1,B,A2,B1),replace_at(R2,B1,A1,B2),retract(march(_)),assertz(march(B2)),retract(vl(Z2,V)),V1 is V-1,assertz(vl(Z2,V1)),checkvide,checkvide,!.

/*meme chose

ce prédicat aide à compter les points des joueurs*/
p(Vl,[X|Y],Somme):-member([X,V],Vl),p(Vl,Y,Somme1),Somme is Somme1+V.
p(_,[],0).

/*ce boucle va faire loop(joueur1)-->loop(joueur2)-->loop(joueur1) si le longueur de la liste des marchandises est supérieur ou egal à 3*/
looppvp(j1):-
march(X),length(X,L),L>2,
/*confirmer qu'il y a au moins 3 piles*/
write('joueur 1'),nl,write('deplace?'),nl,read(C),nl,write('on prends?'),nl,read(Prend),nl,
/*demander à joueur le coup qu'il prendra*/
coup(j1,C,Prend,_),
/*jouer ce coup*/
findall([A,B,R],(march(R1),nth(A,R1,[B|R2]),length(R2,R)),F),write('marchandises:'),nl,affiche_plateau(F),findall([A1,B1],vl(A1,B1),F1),write('valeurs:'),nl,affiche_plateau(F1),findall([Msd,Val],vl(Msd,Val),Vlr),findall([A2,B2,'score:',C2],(res(A2,B2),p(Vlr,B2,C2)),F2),write('reserves:'),nl,affiche_plateau(F2),pile(Pile),write('pile:'),write(Pile),nl,
/*afficher les tableaux*/
looppvp(j2),!.
/*faire passer au joueur 2*/


looppvp(j2):-
march(X),length(X,L),L>2,
write('joueur 2'),nl,write('deplace?'),nl,read(C),nl,write('on prends?'),nl,read(Prend),nl,
coup(j2,C,Prend,_),
findall([A,B,R],(march(R1),nth(A,R1,[B|R2]),length(R2,R)),F),write('marchandises:'),nl,affiche_plateau(F),findall([A1,B1],vl(A1,B1),F1),write('valeurs:'),nl,affiche_plateau(F1),findall([Msd,Val],vl(Msd,Val),Vlr),findall([A2,B2,'score:',C2],(res(A2,B2),p(Vlr,B2,C2)),F2),write('reserves:'),nl,affiche_plateau(F2),pile(Pile),write('pile:'),write(Pile),nl
,looppvp(j1),!.
/*meme chose pour joueur 2

si il y a moins de 3 piles, on va comparer les points des deux joueurs et afficher le gagneur */
looppvp(_):-
march(X),length(X,L),L<3,
/*confirmer qu'il n'y a plus au moins 3 piles*/
write('it''s over'),nl,
/*afficher c'est fini*/
res(j1,P1),res(j2,P2),findall([Msd,Val],vl(Msd,Val),Vlr),p(Vlr,P1,P11),p(Vlr,P2,P22),P22>P11,
/*si joueur 2 gagne*/
write('joueur 2 win'),nl,!,
findall([A2,B2,'score:',C2],(res(A2,B2),p(Vlr,B2,C2)),F2),write('reserves:'),nl,affiche_plateau(F2),!.
/*affiche les reserve et les points des deux joueurs*/
looppvp(_):-march(X),length(X,L),L<3,write('it''s over'),nl,res(j1,P1),res(j2,P2),findall([Msd,Val],vl(Msd,Val),Vlr),p(Vlr,P1,P11),p(Vlr,P2,P22),P11>P22,write('joueur 1 win'),nl,!,findall([A2,B2,'score:',C2],(res(A2,B2),p(Vlr,B2,C2)),F2),write('reserves:'),nl,affiche_plateau(F2),!.
looppvp(_):-march(X),length(X,L),L<3,write('it''s over'),nl,res(j1,P1),res(j2,P2),findall([Msd,Val],vl(Msd,Val),Vlr),p(Vlr,P1,P11),p(Vlr,P2,P22),P11=P22,write('no one wins'),nl,!,findall([A2,B2,'score:',C2],(res(A2,B2),p(Vlr,B2,C2)),F2),write('reserves:'),nl,affiche_plateau(F2),!.
/*meme chose si joueur 1 gagne ou les points sont égales*/


looppve(j1):-march(X),length(X,L),L>2,write('joueur 1'),nl,write('deplace?'),nl,read(C),nl,write('on prends?'),nl,read(Prend),nl,coup(j1,C,Prend,_),looppve(j2),!.
looppve(j2):-
march(X),length(X,L),L>2,
best(C,Prend,Jeter),coup(j2,C,Prend,Jeter),
/*choisir le meilleur choix et jouer le coup*/
write('ordinateur: deplace:'),write(C),write(' on prends '),write(Prend),write(' et on jete '),write(Jeter),nl,
findall([A,B,R],(march(R1),nth(A,R1,[B|R2]),length(R2,R)),F),write('marchandises:'),nl,affiche_plateau(F),findall([A1,B1],vl(A1,B1),F1),write('valeurs:'),nl,affiche_plateau(F1),findall([A2,B2,'score:',C2],(res(A2,B2),findall([Msd,Val],vl(Msd,Val),Vlr),p(Vlr,B2,C2)),F2),write('reserves:'),nl,affiche_plateau(F2),pile(Pile),write('pile:'),write(Pile),nl,looppve(j1),!.

/*meme chose*/
looppve(_):-march(X),length(X,L),L<3,write('it''s over'),nl,res(j1,P1),res(j2,P2),findall([Msd,Val],vl(Msd,Val),Vlr),p(Vlr,P1,P11),p(Vlr,P2,P22),P22>P11,write('joueur 2 win'),nl,!,findall([A2,B2,'score:',C2],(res(A2,B2),p(Vlr,B2,C2)),F2),write('reserves:'),nl,affiche_plateau(F2),!.
looppve(_):-march(X),length(X,L),L<3,write('it''s over'),nl,res(j1,P1),res(j2,P2),findall([Msd,Val],vl(Msd,Val),Vlr),p(Vlr,P1,P11),p(Vlr,P2,P22),P11>P22,write('joueur 1 win'),nl,!,findall([A2,B2,'score:',C2],(res(A2,B2),p(Vlr,B2,C2)),F2),write('reserves:'),nl,affiche_plateau(F2),!.
looppve(_):-march(X),length(X,L),L<3,write('it''s over'),nl,res(j1,P1),res(j2,P2),findall([Msd,Val],vl(Msd,Val),Vlr),p(Vlr,P1,P11),p(Vlr,P2,P22),P11=P22,write('no one wins'),nl,!,findall([A2,B2,'score:',C2],(res(A2,B2),p(Vlr,B2,C2)),F2),write('reserves:'),nl,affiche_plateau(F2),!.

/*fc est un prédicat pour calculer la diffirence entre les points des deux joueurs si joueur2 prends Z1 et jete Z2*/
fc(Z1,Z2,Point):- \+ Z1=Z2,res(j1,R1),res(j2,R2),findall([Msd,Val],vl(Msd,Val),Vlr),p(Vlr,R1,P1),p(Vlr,R2,P2),count(Z2,R1,C1),count(Z2,R2,C2),vl(Z1,V),Point is P2+V+C1-C2-P1.
fc(Z1,Z1,Point):-res(j1,R1),res(j2,R2),findall([Msd,Val],vl(Msd,Val),Vlr),p(Vlr,R1,P1),p(Vlr,R2,P2),count(Z1,R1,C1),count(Z1,R2,C2),vl(Z1,V),Point is P2+V+C1-C2-P1-1 .

/*try est la meilleur choix pour joueur 2 à jouer, ici on prends le coup pour que la diffirence des points soit le plus grande*/
best(C,Prend,Jeter):-march(M1),res(j1,R1),res(j2,R2),findall([A,B],vl(A,B),Vl),pile(Pile),findall(Point,try(4,max,M1,R1,R2,Vl,Pile,Point,_,_,_),P),max_list(P,Best),try(4,max,M1,R1,R2,Vl,Pile,Best,C,Prend,Jeter),!.
try(X,max,M1,R1,R2,Vl,Pile,Point,Y,Z1,Z2):- X>0,length(M1,Len),Len>3,findall(Dif,possible_coup(M1,R1,R2,Vl,Pile,j2,_,_,_,_,_,_,_,_,Dif),F),X1 is X-1,max_list(F,Difmax),possible_coup(M1,R1,R2,Vl,Pile,j2,Y,Z1,Z2,M2,R12,R21,Vl2,P2,Difmax),try(X1,min,M2,R12,R21,Vl2,P2,Point,_,_,_).
try(X,min,M1,R1,R2,Vl,Pile,Point,_,_,_):- X>0,length(M1,Len),Len>3,findall(Dif,possible_coup(M1,R1,R2,Vl,Pile,j1,_,_,_,_,_,_,_,_,Dif),F),X1 is X-1,min_list(F,Difmin),possible_coup(M1,R1,R2,Vl,Pile,j1,Y,Z1,Z2,M2,R12,R21,Vl2,P2,Difmin),try(X1,max,M2,R12,R21,Vl2,P2,Point,_,_,_).
try(0,max,M1,R1,R2,Vl,Pile,Point,_,_,_):- length(M1,Len),Len>3,findall(Dif,possible_coup(M1,R1,R2,Vl,Pile,j2,_,_,_,_,_,_,_,_,Dif),F),max_list(F,Point).
try(_,max,M1,R1,R2,Vl,Pile,Point,_,_,_):- length(M1,Len),Len<4,findall(Dif,possible_coup(M1,R1,R2,Vl,Pile,j2,_,_,_,_,_,_,_,_,Dif),F),max_list(F,Point).
try(_,min,M1,R1,R2,Vl,Pile,Point,_,_,_):- length(M1,Len),Len<4,findall(Dif,possible_coup(M1,R1,R2,Vl,Pile,j1,_,_,_,_,_,_,_,_,Dif),F),min_list(F,Point).