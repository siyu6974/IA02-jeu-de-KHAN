% Projet IA02
:- include('movementRules.pl').
:- include('jeuDeKHAN_lib.pl').
% printf("The result is %q, which is %d%% better than %w\n%b", [T, P, R]).

% Etape 1
terrainMap([2,3,1,2,2,3,2,1,3,1,3,1,1,3,2,3,1,2,3,1,2,1,3,2,2,3,1,3,1,3,2,1,3,2,2,1]).
printMatrix([],_).
printMatrix(L,0) :- nl,write(' '),printMatrix(L,6),!.
printMatrix([H|T],Count) :- write(' '),write(H),  Next is Count-1, printMatrix(T,Next),!.
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

afficherBoard:-board(TerrainMap,_,_),write(' '),printMatrix(TerrainMap,6),
				nl,board(_,BF,_), printBattleField(-1,0,BF),!.

initBoard :-
	terrainMap(TerrainMap),
    asserta(board(TerrainMap,[24, 31, 18, 19, 1, 25, 16, 35, 17, 10, 5, 23],1)).

% tryMove prevents friendly fire
tryMove(Pos,Result):-
	board(TerrainMap,BF,KHAN), nth0(Pos,TerrainMap,Step),%obeying terrainMap
	(
	KHAN == Step;
	KHAN == 0
	),
	possMove(Step,Pos,Tmp),indexOf(BF,Pos,N),
		(
		N=<6,%side 1 moving
		sublistOf(1,6,BF,Side1),subtract(Tmp,Side1,Result);
		N>6,%side 2 moving
		sublistOf(7,12,BF,Side2),subtract(Tmp,Side2,Result)
		),!.

% move gives the consiquence of a move
move(Pos,Dest) :- board(TerrainMap,BF,_), indexOf(BF,Pos,N),
	(
	N=<6,%side 1 moving
	sublistOf(7,12,BF,Side2),member(Dest,Side2),
	modifyList(Dest,44,BF,NewBF),modifyList(Pos,Dest,NewBF,FinBF); %capture

	N>6,%side 2 moving
	sublistOf(1,6,BF,Side1),member(Dest,Side1),
	modifyList(Dest,44,BF,NewBF),modifyList(Pos,Dest,NewBF,FinBF);%capture

	modifyList(Pos,Dest,BF,FinBF)%peace
	),
	nth0(Dest,TerrainMap,NewKhan),
	retract(board(_,BF,_)),%delete old board,
	asserta(board(TerrainMap,FinBF,NewKhan)),!.


main:-initBoard,choosemode.
% UI.
choosemode:-
	write('Choose the mode of play:' ), nl,
	write('1 Human VS Computer'),nl,
	write('2 Computer VS Human'),nl,
	write('3 Human VS Human'),nl,
	write('4 Computer VS Computer'),nl,
	write('Type mode(Choice) to enter your choice please.').

mode(1):-write('Human VS Computer, Good luck!'),nl,afficherBoard.
mode(2):-write('Computer VS Human, Good luck!'),nl,afficherBoard.
mode(3):-write('Human VS Human, Good luck!'),nl,afficherBoard.
mode(4):-write('Computer VS Computer'),nl,afficherBoard.
