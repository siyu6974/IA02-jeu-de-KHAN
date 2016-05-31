sublistOf(Start,End,List,Sublist) :-
    sublist(Sublist,List),L is End-Start+1, length(Sublist,L),
	nth(1,Sublist,Tmp),nth(Start,List,Tmp),nth(L,Sublist,Tmp2),nth(End,List,Tmp2).

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

% length of a 2 dimensional list, lenTotal(List,0,Len)
lenTotal([],Count,Count).
lenTotal([ElementList|T],Start,Count):-
    length(ElementList,Len),
    Tmp is Start + Len,
    lenTotal(T,Tmp,Count).
