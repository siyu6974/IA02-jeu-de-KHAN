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

main:-afficherBoard.
basicMove(Pos,-6) :- Pos-6>=0.
basicMove(Pos,6) :- Pos+6<36.
basicMove(Pos,-1) :- ((Pos-1) // 6) =:= (Pos//6).
basicMove(Pos,1) :- ((Pos+1) // 6) =:= (Pos//6).

% possMove(Step,Pos,Result)
possMove(1,Pos,Result) :- setof(PossibleMove,basicMove(Pos,PossibleMove),Result).
