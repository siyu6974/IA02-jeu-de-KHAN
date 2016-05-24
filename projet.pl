% Projet IA02
% printf("The result is %q, which is %d%% better than %w\n%b", [T, P, R]).

% Etape 1
printMatrix([],_).
printMatrix(L,0) :- nl,printMatrix(L,6).
printMatrix([H|T],Count) :- write(H), write(' '), Next is Count-1, printMatrix(T,Next).

sublistOf(Start,End,List,Sublist) :- sublist(Sublist,List),L is End-Start+1, length(Sublist,L),
									nth(1,Sublist,Tmp),nth(Start,List,Tmp),nth(L,Sublist,Tmp2),nth(End,List,Tmp2).

printBattleField(-1,0,BF) :- nl, write('   A B C D E F'), nl,  printBattleField(0,0,board).
printBattleField(Line,Row,BF) :- write(Line), Pos is (Line * 6 + Row),
									sublistOf(1,5,BF,Sublist),member(Pos,Sublist),write('o'),!.
printBattleField(Line,Row,BF) :- write(Line),  Pos is (Line * 6 + Row),
									sublistOf(7,11,BF,Sublist),member(Pos,Sublist),write('x'),!.
printBattleField(Line,Row,BF) :- write(Line), Pos is (Line * 6 + Row),
									write('_').


board([2,3,1,2,2,3,2,1,3,1,3,1,1,3,2,3,1,2,3,1,2,1,3,2,2,3,1,3,1,3,2,1,3,2,2,1],[P0,P1,P2,P3,P4,Q1,P6,P7,P8,P9,P10,Q2],KHAN).

afficherBoard:-initBoard,board(TerrainMap,_,_),printMatrix(TerrainMap,6),board(_,BF,_),findall(_,printBattleField(-1,0,BF),_).
test:-initBoard,board(_,T,_),printMatrix(T,6).
initBoard :-
    board(_,[24, 31, 18, 19, 1, 25, 16, 35, 17, 10, 5, 23],_).
    % printf("Enter the position of the %d pawn", N),
	% read(tmpPosition).

main:-afficherBoard.
