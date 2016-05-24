start:-valeur_depart,plateau_depart,pile_depart,startreserve,findall([A,B,C],(march(M),nth(A,M,[B|X]),length(X,C)),F),write('marchandises:'),nl,affiche_plateau(F),pile(Pile),write('pile:'),write(Pile),nl,loop(j1).
affiche_plateau([A|B]):- \+B=[],write(A),nl,affiche_plateau(B).
affiche_plateau([A|[]]):-write(A),nl,!.
valeur_depart:-retractall(vl(_,_)),assertz(vl(ble,7)),assertz(vl(riz,6)),assertz(vl(cacao,6)),assertz(vl(cafe,6)),assertz(vl(sucre,6)),assertz(vl(mais,6)).
replace_at(Z,[_|Xs],1,[Z|Xs]):-!.
replace_at(X,[Y|Xs],K,[Y|Ys]) :- K > 1,T is (K - 1),replace_at(X,Xs,T,Ys).
remove_at(X,[X|Xs],1,Xs):-!.
remove_at(X,[Y|Xs],K,[Y|Ys]) :- K > 1, T is (K - 1),remove_at(X,Xs,T,Ys).
rnd_select(_,0,[]).
rnd_select(Xs,N,[X|Zs]) :- N > 0,length(Xs,Beibei),L is Beibei+1,random(1,L,I),remove_at(X,Xs,I,Ys),N1 is N - 1,rnd_select(Ys,N1,Zs).
plateau_depart:-retractall(march(_)),randomize,rnd_select([ble,ble,ble,ble,ble,ble,riz,riz,riz,riz,riz,riz,cacao,cacao,cacao,cacao,cacao,cacao,cafe,cafe,cafe,cafe,cafe,cafe,mais,mais,mais,mais,mais,mais,sucre,sucre,sucre,sucre,sucre,sucre],36,[A1,A2,A3,A4,A5,A6,A7,A8,A9,A10,A11,A12,A13,A14,A15,A16,A17,A18,A19,A20,A21,A22,A23,A24,A25,A26,A27,A28,A29,A30,A31,A32,A33,A34,A35,A36]),assertz(march([[A1,A2,A3,A4],[A5,A6,A7,A8],[A9,A10,A11,A12],[A13,A14,A15,A16],[A17,A18,A19,A20],[A21,A22,A23,A24],[A25,A26,A27,A28],[A29,A30,A31,A32],[A33,A34,A35,A36]])),!.
checkvide:-march(A),pile(X),nth(B,A,[]),remove_at([],A,B,Y),X>B,X1 is X-1,retract(pile(_)),assertz(pile(X1)),retract(march(A)),assertz(march(Y)),!.
checkvide:-march(A),pile(X),nth(B,A,[]),remove_at([],A,B,Y),\+(X>B),retract(march(A)),assertz(march(Y)),!.
checkvide:-march(A),\+nth(_,A,[]).
pile_depart:-retractall(pile(_)),assertz(pile(1)).
startreserve:-retractall(res(_,_)),assertz(res(j1,[])),assertz(res(j2,[])).
reserve(X,Y):-member(X,[j1,j2]),retract(res(X,Z)),assertz(res(X,[Y|Z])).
dep(X,N):-pile(A),Y is A+X,march(L),length(L,Z),(Y>Z)->(N is Y-Z),!.
dep(X,N):-pile(A),Y is A+X,march(L),length(L,Z),(Y=<Z)->(N is Y),!.
deplace(A):-member(A,[1,2,3]),dep(A,B),retract(pile(_)),assertz(pile(B)).
possible_coup(X,Y,Z1,Z2):-member(X,[j1,j2]),member(Y,[1,2,3]),march(B),Y1 is Y+1,Y2 is Y-1,dep(Y1,A1),dep(Y2,A2),(nth(A1,B,[Z1|_]),nth(A2,B,[Z2|_])).
possible_coup(X,Y,Z1,Z2):-member(X,[j1,j2]),member(Y,[1,2,3]),march(B),Y1 is Y+1,Y2 is Y-1,dep(Y1,A1),dep(Y2,A2),(nth(A2,B,[Z1|_]),nth(A1,B,[Z2|_])).
coup(X,Y,Z1,Z2):-member(X,[j1,j2]),member(Y,[1,2,3]),march(B),Y1 is Y+1,Y2 is Y-1,dep(Y1,A1),dep(Y2,A2),(nth(A1,B,[Z1|R1]),nth(A2,B,[Z2|R2])),deplace(Y),reserve(X,Z1),replace_at(R1,B,A1,B1),replace_at(R2,B1,A2,B2),retract(march(_)),assertz(march(B2)),retract(vl(Z2,V)),V1 is V-1,assertz(vl(Z2,V1)),checkvide,checkvide,!.
coup(X,Y,Z1,Z2):-member(X,[j1,j2]),member(Y,[1,2,3]),march(B),Y1 is Y+1,Y2 is Y-1,dep(Y1,A1),dep(Y2,A2),(nth(A2,B,[Z1|R1]),nth(A1,B,[Z2|R2])),deplace(Y),reserve(X,Z1),replace_at(R1,B,A2,B1),replace_at(R2,B1,A1,B2),retract(march(_)),assertz(march(B2)),retract(vl(Z2,V)),V1 is V-1,assertz(vl(Z2,V1)),checkvide,checkvide,!.
p([X|Y],Somme):-vl(X,V),p(Y,Somme1),Somme is Somme1+V.
p([],0).
loop(j1):-march(X),length(X,L),L>2,write('joueur 1'),nl,write('deplace?'),nl,read(C),nl,write('on prends?'),nl,read(Prend),nl,coup(j1,C,Prend,_),findall([A,B,R],(march(R1),nth(A,R1,[B|R2]),length(R2,R)),F),write('marchandises:'),nl,affiche_plateau(F),findall([A1,B1],vl(A1,B1),F1),write('valeurs:'),nl,affiche_plateau(F1),findall([A2,B2,'score:',C2],(res(A2,B2),p(B2,C2)),F2),write('reserves:'),nl,affiche_plateau(F2),pile(Pile),write('pile:'),write(Pile),nl,loop(j2).
loop(j2):-march(X),length(X,L),L>2,write('joueur 2'),nl,write('deplace?'),nl,read(C),nl,write('on prends?'),nl,read(Prend),nl,coup(j2,C,Prend,_),findall([A,B,R],(march(R1),nth(A,R1,[B|R2]),length(R2,R)),F),write('marchandises:'),nl,affiche_plateau(F),findall([A1,B1],vl(A1,B1),F1),write('valeurs:'),nl,affiche_plateau(F1),findall([A2,B2,'score:',C2],(res(A2,B2),p(B2,C2)),F2),write('reserves:'),nl,affiche_plateau(F2),pile(Pile),write('pile:'),write(Pile),nl,loop(j1).
loop(_):-march(X),length(X,L),L=2,write('it''s over'),nl,res(j1,P1),res(j2,P2),p(P1,P11),p(P2,P22),P22>P11,write('joueur 2 win'),nl,!,findall([A2,B2,'score:',C2],(res(A2,B2),p(B2,C2)),F2),write('reserves:'),nl,affiche_plateau(F2),!.
loop(_):-march(X),length(X,L),L=2,write('it''s over'),nl,res(j1,P1),res(j2,P2),p(P1,P11),p(P2,P22),P11>P22,write('joueur 1 win'),nl,!,findall([A2,B2,'score:',C2],(res(A2,B2),p(B2,C2)),F2),write('reserves:'),nl,affiche_plateau(F2),!.
loop(_):-march(X),length(X,L),L=2,write('it''s over'),nl,res(j1,P1),res(j2,P2),p(P1,P11),p(P2,P22),P11=P22,write('no one wins'),nl,!,findall([A2,B2,'score:',C2],(res(A2,B2),p(B2,C2)),F2),write('reserves:'),nl,affiche_plateau(F2),!.
