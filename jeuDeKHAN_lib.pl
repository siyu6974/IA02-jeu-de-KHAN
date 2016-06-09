% ---------------Supporting Predicate------------
% oppositeNb(1,Y).
oppositeNb(X,Y):- Y is -X.

max(A,B,A):-A>=B.
max(A,B,B):-B>A.
min(A,B,A):-A=<B.
min(A,B,B):-B<A.

%slice(Start,End,List,Sublist)
slice(1,1,[H|_],[H]).
slice(1,End,[H|T1],[H|T2]) :-
    End > 1, EndT is End - 1, slice(1,EndT,T1,T2).
slice(Start,End,[_|T],Sublist) :-
    Start > 1, StartT is Start - 1,
    EndT is End - 1, slice(StartT,EndT,T,Sublist).

%start at 0, indexOf(List,Element,Index)
indexOf([Element|_], Element, 0).
indexOf([_|Tail], Element, Index):-
	  indexOf(Tail, Element, Index1),
	  Index is Index1+1.

mapFunc(Target,NewValue,N,Return):-
  (N=:=Target,Return is NewValue,!;
	Return is N).

% modifyList scans the List, changing the first case whose value is Target to NewValue,
% and returns NewList
modifyList(Target,NewValue,List,NewList):-
    maplist(mapFunc(Target,NewValue),List,NewList).

% modifyList2(Index,Dest,List,Result,Count)
% changes List[Index] to Dest, returns Result.
modifyList2(_,_,_,[],12).
modifyList2(Index,Dest,[E|Q1],[R|Q2],Count):-
	(
	   Index==Count, R is Dest
    ;
	   R is E
	),
	Next is Count + 1,
	modifyList2(Index,Dest,Q1,Q2,Next),!.

% length of a 2 dimensional list, lenTotal(List,Len)
lenTotal([],0).
lenTotal([ElementList|T],Count):-
    length(ElementList,Len),
    lenTotal(T,Tmp),
    Count is Tmp+Len.

% distributer(2dList,Return) [[P,1,2,3],[Q,1,2,3]]->[[P,1],[P,2]...[Q,3]]
distributer([],[]).
distributer([[H|R]|Tail],Return):-
    subdistributer(H,R,L1),
    append(L1,NextGroup,Return),
    distributer(Tail,NextGroup).

% subdistributer(P,1dList,Return) [P,1,2,3]->[[P,1],[P,2],[P,3]]
subdistributer(_,[],[]):-!.
subdistributer(H,[S|RList],[[H,S]|NextPair]):-
    subdistributer(H,RList,NextPair).

% regroup([L1,L2],L1,L2) 
regroup(L,L1,L2):-
    maplist(nth(1),L,L1),
    maplist(nth(2),L,L2).

% choose(List, Elt) - chooses a random element
choose([], []).
choose(List, Elt) :-
    length(List, Length),
    random(0, Length, Index),
    nth0(Index, List, Elt),!.

printMatrix([],_).
printMatrix(L,0) :- nl,write(' '),printMatrix(L,6),!.
printMatrix([H|T],Count) :- write(' '),write(H),  Next is Count-1, printMatrix(T,Next),!.
printLineNumber(6,_):-!.
printLineNumber(Line,BF) :- nl, write(Line), write(' '), printBattleField(Line,0,BF).
