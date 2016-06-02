move1(Pos,-6,Dest) :- Dest is Pos-6, Dest>=0.
move1(Pos,6,Dest) :- Dest is Pos+6, Dest<36.
move1(Pos,-1,Dest) :- Dest is Pos-1, Dest>=0, (Dest // 6) =:= (Pos//6).
move1(Pos,1,Dest) :- Dest is Pos+1, Dest<36, (Dest // 6) =:= (Pos//6).

move2(Pos,Dest,BF):-
    member(Dir,[-1,1,-6,6]),move1(Pos,Dir,T),\+(member(T,BF)), oppositeNb(Dir,NegDir),
    select(NegDir,[-1,1,-6,6],PossDir),member(Dir2,PossDir),move1(T,Dir2,Dest).

move3(Pos,Dest,BF):-
    member(Dir,[-1,1,-6,6]),move1(Pos,Dir,T),\+(member(T,BF)), oppositeNb(Dir,NegDir),
    select(NegDir,[-1,1,-6,6],PossDir),member(Dir2,PossDir),move1(T,Dir2,Tmp),
    \+(member(Tmp,BF)),oppositeNb(Dir2,NegDir2),select(NegDir2,[-1,1,-6,6],PossDir2),
    member(Dir3,PossDir2),move1(Tmp,Dir3,Dest).

possMove(1,Pos,Result) :- setof(Dest,Dir^move1(Pos,Dir,Dest),Result).
possMove(2,Pos,Result) :- board(_,BF,_),setof(Dest,move2(Pos,Dest,BF),Result).
possMove(3,Pos,Result) :- board(_,BF,_),setof(Dest,move3(Pos,Dest,BF),Result).
