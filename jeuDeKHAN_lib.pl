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
