% TP1-IA02
% EX01
pere(jean,pierre).
pere(jean,jacques).
pere(pierre,paul).
pere(jacques,remi).
grandpere(X,Z) :- pere(X,Y),pere(Y,Z).
% EX02
age(paul,20).
age(pierre, 30).
age(julie, 15).
plus_vieux(X,Y):- age(X,P),
age(Y,Q),
P>Q.
% EX03
fact(0,1).
fact(N,F) :- N>=0, X is N-1, fact(X,R), F is N * R.

% EX04
test(1).
test(2).
test2(X) :- test(X),!.
non(X) :- X, fail, ! .
non(_).
diff(A,B) :- non(A == B).

% EX05
liste([]).
liste([X|Y]) :- liste(Y).

app(Elem, [Elem|_]):- !. % Si l'element est la tete.
% ! On trouve la valeur, on cut puis on sort(On ne cherche pas les restes).
app(Elem, [_|Tail]) :- app(Elem, Tail). % Sinon on enleve la tete et cherche dans la tail.

% EX06
concat([],L,L).
concat([H|T],L2,[H|L3]) :- concat(T,L2,L3).

% EX07
longueur([], 0).
longueur([_|T], Len) :- longueur(T,Tmp), Len is Tmp+1 .

% EX08
elementNumN([H|T], 1, H).
elementNumN([H|T], N, X) :- longueur([H|T],Len), N=<Len, Tmp is N-1, elementNumN(T,Tmp,X).