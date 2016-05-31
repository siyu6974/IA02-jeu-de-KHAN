% Projet IA02
% printf("The result is %q, which is %d%% better than %w\n%b", [T, P, R]).

% Etape 1
terrainMap([2,3,1,2,2,3,2,1,3,1,3,1,1,3,2,3,1,2,3,1,2,1,3,2,2,3,1,3,1,3,2,1,3,2,2,1]).
printMatrix([],_).
printMatrix(L,0) :- nl,write(' '),printMatrix(L,6),!.
printMatrix([H|T],Count) :- write(' '),write(H),  Next is Count-1, printMatrix(T,Next),!.

sublistOf(Start,End,List,Sublist) :- sublist(Sublist,List),L is End-Start+1, length(Sublist,L),
									nth(1,Sublist,Tmp),nth(Start,List,Tmp),nth(L,Sublist,Tmp2),nth(End,List,Tmp2).
printLineNumber(6,_):-!.
printLineNumber(Line,BF) :- nl, write(Line), write(' '), printBattleField(Line,0,BF).

printBattleField(-1,0,BF) :- nl, write('  A B C D E F'),  printLineNumber(0,BF),!.
printBattleField(Line,6,BF) :- NextLine is Line+1, printLineNumber(NextLine,BF).
printBattleField(Line,Row,BF) :- Scanning is Line*6+Row,
							(sublistOf(1,5,BF,Side1),member(Scanning,Side1), write('o ');
							sublistOf(7,11,BF,Side2),member(Scanning,Side2), write('x ');
							nth(6,BF,Q1),Q1==Scanning, write('O ');
							nth(12,BF,Q2),Q2==Scanning,write('X ');
							write('_ ')),
							Tmp is Row+1, printBattleField(Line,Tmp,BF).
:- dynamic(board/3).
% board(TerrainMap,[P0,P1,P2,P3,P4,Q1,P6,P7,P8,P9,P10,Q2],KHAN):-terrainMap(TerrainMap).

afficherBoard:-initBoard,board(TerrainMap,_,_),write(' '),printMatrix(TerrainMap,6),
				nl,board(_,BF,_), printBattleField(-1,0,BF).
test:-initBoard,board(_,T,_),printMatrix(T,6).
initBoard :-
	terrainMap(TerrainMap),
    asserta(board(TerrainMap,[24, 31, 18, 19, 1, 25, 16, 35, 17, 10, 5, 23],0)).
    % printf("Enter the position of the %d pawn", N),
	% read(tmpPosition).

move1(Pos,-6,Dest) :- Dest is Pos-6, Dest>=0.
move1(Pos,6,Dest) :- Dest is Pos+6, Dest<36.
move1(Pos,-1,Dest) :- Dest is Pos-1, (Dest // 6) =:= (Pos//6).
move1(Pos,1,Dest) :- Dest is Pos+1, (Dest // 6) =:= (Pos//6).
move2(Pos,-12,Dest,BF):-move1(Pos,-6,T),\+(member(T,BF)),move1(T,-6,Dest).
move2(Pos,12,Dest,BF):-move1(Pos,6,T),\+(member(T,BF)),move1(T,6,Dest).
move2(Pos,-7,Dest,BF):-move1(Pos,-6,T),\+(member(T,BF)),move1(T,-1,Dest).
move2(Pos,-7,Dest,BF):-move1(Pos,-1,T),\+(member(T,BF)),move1(T,-6,Dest).
move2(Pos,-2,Dest,BF):-move1(Pos,-1,T),\+(member(T,BF)),move1(T,-1,Dest).
move2(Pos,2,Dest,BF):-move1(Pos,1,T),\+(member(T,BF)),move1(T,1,Dest).
move2(Pos,-5,Dest,BF):-move1(Pos,-6,T),\+(member(T,BF)),move1(T,1,Dest).
move2(Pos,-5,Dest,BF):-move1(Pos,1,T),\+(member(T,BF)),move1(T,-6,Dest).
move2(Pos,5,Dest,BF):-move1(Pos,6,T),\+(member(T,BF)),move1(T,-1,Dest).
move2(Pos,5,Dest,BF):-move1(Pos,-1,T),\+(member(T,BF)),move1(T,6,Dest).
move2(Pos,7,Dest,BF):-move1(Pos,6,T),\+(member(T,BF)),move1(T,1,Dest).
move2(Pos,7,Dest,BF):-move1(Pos,1,T),\+(member(T,BF)),move1(T,6,Dest).

move3(Pos,-18,Dest,BF):-move2(Pos,-12,T),\+(member(T,BF)),move1(T,-6,Dest).
move3(Pos,-13,Dest,BF):-move2(Pos,-12,T),\+(member(T,BF)),move1(T,-1,Dest).
move3(Pos,-13,Dest,BF):-move2(Pos,-7,T),\+(member(T,BF)),move1(T,-6,Dest).
move3(Pos,-8,Dest,BF):-move2(Pos,-7,T),\+(member(T,BF)),move1(T,-1,Dest).
move3(Pos,-8,Dest,BF):-move2(Pos,-2,T),\+(member(T,BF)),move1(T,-6,Dest).
move3(Pos,-4,Dest,BF):-move2(Pos,-5,T),\+(member(T,BF)),move1(T,1,Dest).
move3(Pos,-4,Dest,BF):-move2(Pos,2,T),\+(member(T,BF)),move1(T,-6,Dest).
move3(Pos,-6,Dest,BF):-move2(Pos,-7,T),\+(member(T,BF)),move1(T,1,Dest).
move3(Pos,-6,Dest,BF):-move2(Pos,-5,T),\+(member(T,BF)),move1(T,-1,Dest).

move3(Pos,18,Dest,BF):-move2(Pos,12,T),\+(member(T,BF)),move1(T,6,Dest).
move3(Pos,13,Dest,BF):-move2(Pos,12,T),\+(member(T,BF)),move1(T,1,Dest).
move3(Pos,13,Dest,BF):-move2(Pos,7,T),\+(member(T,BF)),move1(T,6,Dest).
move3(Pos,8,Dest,BF):-move2(Pos,7,T),\+(member(T,BF)),move1(T,1,Dest).
move3(Pos,8,Dest,BF):-move2(Pos,2,T),\+(member(T,BF)),move1(T,6,Dest).
move3(Pos,4,Dest,BF):-move2(Pos,5,T),\+(member(T,BF)),move1(T,-1,Dest).
move3(Pos,4,Dest,BF):-move2(Pos,-2,T),\+(member(T,BF)),move1(T,6,Dest).
move3(Pos,6,Dest,BF):-move2(Pos,7,T),\+(member(T,BF)),move1(T,-1,Dest).
move3(Pos,6,Dest,BF):-move2(Pos,5,T),\+(member(T,BF)),move1(T,1,Dest).

move3(Pos,-3,Dest,BF):-move2(Pos,-2,T),\+(member(T,BF)),move1(T,-1,Dest).
move3(Pos,-1,Dest,BF):-move2(Pos,-7,T),\+(member(T,BF)),move1(T,6,Dest).
move3(Pos,-1,Dest,BF):-move2(Pos,5,T),\+(member(T,BF)),move1(T,-6,Dest).
move3(Pos,3,Dest,BF):-move2(Pos,2,T),\+(member(T,BF)),move1(T,1,Dest).
move3(Pos,1,Dest,BF):-move2(Pos,7,T),\+(member(T,BF)),move1(T,-6,Dest).
move3(Pos,1,Dest,BF):-move2(Pos,-5,T),\+(member(T,BF)),move1(T,6,Dest).


test(T,R):-R is T+1.
% possMove(1,Pos,Result) :- setof(PossibleMove,basicMove(Pos,PossibleMove),Result).
% possMove(Step,Pos,Result)
possMove(1,Pos,Result) :- setof(Dest,Dir^move1(Pos,Dir,Dest),Result).
possMove(2,Pos,Result) :- board(_,BF,_),setof(Dest,Dir^move2(Pos,Dir,Dest,BF),Result).
possMove(3,Pos,Result) :- board(_,BF,_),setof(Dest,Dir^move3(Pos,Dir,Dest,BF),Result).
main:-choosemode.

% Decide players.
choosemode:-
	write('Choose the mode of play:' ), nl,
	write('1 Human VS Computer'),nl,
	write('2 Human VS Human'),nl,
	write('3 Computer VS Computer'),nl,
	write('Use the fonction mode(Choice) to enter your choice please.').

mode(1):-write('Human VS Computer, Good luck!'),nl,afficherBoard.
mode(2):-write('Human VS Human, Good luck!'),nl,afficherBoard.
mode(3):-write('Computer VS Computer'),nl,afficherBoard.
