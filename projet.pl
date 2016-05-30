% Projet IA02
% printf("The result is %q, which is %d%% better than %w\n%b", [T, P, R]).

% Etape 1
terrainMap([2,3,1,2,2,3,2,1,3,1,3,1,1,3,2,3,1,2,3,1,2,1,3,2,2,3,1,3,1,3,2,1,3,2,2,1]).
printMatrix([],_).
printMatrix(L,0) :- nl,printMatrix(L,6).
printMatrix([H|T],Count) :- write(H), write(' '), Next is Count-1, printMatrix(T,Next).

sublistOf(Start,End,List,Sublist) :- sublist(Sublist,List),L is End-Start+1, length(Sublist,L),
									nth(1,Sublist,Tmp),nth(Start,List,Tmp),nth(L,Sublist,Tmp2),nth(End,List,Tmp2).

printBattleField(-1,0,BF) :- nl, write('   A B C D E F'), nl,  printLineNumber(0,BF).
printLineNumber(Line,BF) :- write(Line), write(' '), printBattleField(Line,0,BF).
printBattleField(Line,Row,BF) :- Row == 7, NextLine is Line+1, printLineNumber(NextLine,0,BF).
printBattleField(Line,Row,BF) :- sublistOf(1,5,BF,Side1), Scanning is Line*6+Row, member(Scanning,BF), write('o '),Tmp is Line+1,
								printBattleField(Line,Row,BF).
printBattleField(Line,Row,BF) :- sublistOf(7,11,BF,Side1), Scanning is Line*6+Row, member(Scanning,BF), write('x '),Tmp is Line+1,
								printBattleField(Line,Row,BF).
printBattleField(Line,Row,BF) :- sublistOf(1,5,BF,Side1), write('_ '),Tmp is Line+1,
																printBattleField(Line,Row,BF).
board(TerrainMap,[P0,P1,P2,P3,P4,Q1,P6,P7,P8,P9,P10,Q2],KHAN):-terrainMap(TerrainMap).


afficherBoard:-initBoard,board(TerrainMap,_,_),printMatrix(TerrainMap,6),board(_,BF,_), printBattleField(-1,0,BF).
test:-initBoard,board(_,T,_),printMatrix(T,6).
initBoard :-
    asserta(board(_,[24, 31, 18, 19, 1, 25, 16, 35, 17, 10, 5, 23],_)).
    % printf("Enter the position of the %d pawn", N),
	% read(tmpPosition).

main:-afficherBoard.
