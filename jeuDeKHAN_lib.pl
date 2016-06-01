%slice(Start,End,List,Sublist)
slice(1,1,[H|_],[H]).
slice(1,End,[H|T1],[H|T2]) :-
    End > 1, EndT is End - 1, slice(1,EndT,T1,T2).
slice(Start,End,[_|T],Sublist) :-
    Start > 1, StartT is Start - 1,
    EndT is End - 1, slice(StartT,EndT,T,Sublist).

indexOf([Element|_], Element, 0).
indexOf([_|Tail], Element, Index):-
	  indexOf(Tail, Element, Index1),
	  Index is Index1+1.

mapFunc(Target,NewValue,N,Return):-
    (N=:=Target,Return is NewValue,!;
	Return is N).

modifyList(Target,NewValue,List,NewList):-
    maplist(mapFunc(Target,NewValue),List,NewList).

% modifyList2(Index,Dest,List,Result,Count):-
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
