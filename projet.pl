% Projet IA02
:- include('movementRules.pl').
:- include('jeuDeKHAN_lib.pl').

:- dynamic(board/3).

terrainMap([2,3,1,2,2,3,2,1,3,1,3,1,1,3,2,3,1,2,3,1,2,1,3,2,2,3,1,3,1,3,2,1,3,2,2,1]).
printMatrix([],_).
printMatrix(L,0) :- nl,write(' '),printMatrix(L,6),!.
printMatrix([H|T],Count) :- write(' '),write(H),  Next is Count-1, printMatrix(T,Next),!.
printLineNumber(6,_):-!.
printLineNumber(Line,BF) :- nl, write(Line), write(' '), printBattleField(Line,0,BF).

printBattleField(-1,0,BF) :-
	nl, write('  A B C D E F'),  printLineNumber(0,BF),!.
printBattleField(Line,6,BF) :-
	NextLine is Line+1, printLineNumber(NextLine,BF).
printBattleField(Line,Row,BF) :-
	Scanning is Line*6+Row,
	(
		sublistOf(1,5,BF,Side0),member(Scanning,Side0), write('o ')
	;
		sublistOf(7,11,BF,Side1),member(Scanning,Side1), write('x ')
	;
		nth(6,BF,Q1),Q1==Scanning, write('O ')
	;
		nth(12,BF,Q2),Q2==Scanning,write('X ')
	;
		write('_ ')
	),
	Tmp is Row+1, printBattleField(Line,Tmp,BF).

printDeadPiece(BF):- findall(Index,
	(indexOf(BF,44,Index),
		(
			Index=<5,write('o ')
		;
			Index>5,write('x ')
	)),
	_).

afficherBoard:-
	board(TerrainMap,_,KHAN),write(' '),printMatrix(TerrainMap,6),
	nl,board(_,BF,_), printBattleField(-1,0,BF),nl,write('   '),
	printDeadPiece(BF),!,nl,write('KHAN = '),write(KHAN).


initBoard :-
	terrainMap(TerrainMap),
    asserta(board(TerrainMap,[44, 44, 18, 20, 1, 25, 16, 44, 17, 10, 5, 23],1)).

% allPossibleMove(Side,Result)
allPossibleMove(Side,Result):-
	board(TerrainMap,BF,KHAN),
	(
		Side == 0,sublistOf(1,6,BF,SideP)
	;
		sublistOf(7,12,BF,SideP)
	),!,
	findall(
		[Piece|PossibleMove],
		(member(Piece,SideP),tryMove(Piece,TerrainMap,BF,KHAN,PossibleMove)),
		R1),
	(
		length(R1,Len),
		%KHAN sucks
		(
			Len==0%KHAN sucks because of terrain
		;
			lenTotal(R1,0,Len2),%KHAN sucks because of terrain+blocage
			Len==Len2			%eg: R1=[[17]], 17 could move but blocked
		),!,
		(
			resurrectionTarget(BF,1,_),
			resurrectionPosition(BF,TerrainMap,KHAN,GardenTomb),
			R2 = [[44|GardenTomb]]
		;
			R2 =[]
		),!,
		findall(
			[Piece|PossibleMove],
			(member(Piece,SideP),tryMove(Piece,TerrainMap,BF,0,PossibleMove)),
			R1n),
		append(R1n,R2,Result)
	;
		%loyal suject of KHAN
		Result = R1
	),!.
% tryMove prevents friendly fire
tryMove(Pos,TerrainMap,BF,KHAN,Result):-
	Pos<36, nth0(Pos,TerrainMap,Step),%obeying terrainMap
	(
		KHAN == Step
	;
		KHAN == 0
	),
	possMove(Step,Pos,Tmp),indexOf(BF,Pos,N),
	(
		N=<5,%side 0
		sublistOf(1,6,BF,Side0),subtract(Tmp,Side0,Result)
	;
		N>5,%side 1
		sublistOf(7,12,BF,Side1),subtract(Tmp,Side1,Result)
	),!.

% move gives the consiquence of a move
move(Pos,Dest) :-
	board(TerrainMap,BF,_), indexOf(BF,Pos,N),
	(
		N=<5,%side 0 moving
		sublistOf(7,12,BF,Side1),member(Dest,Side1),
		modifyList(Dest,44,BF,NewBF), modifyList(Pos,Dest,NewBF,FinBF)%capture
	;
		N>5,%side 1 moving
		sublistOf(1,6,BF,Side0),member(Dest,Side0),
		modifyList(Dest,44,BF,NewBF), modifyList(Pos,Dest,NewBF,FinBF)%capture
	;
		modifyList(Pos,Dest,BF,FinBF)%peace
	),
	nth0(Dest,TerrainMap,NewKhan),
	retract(board(_,BF,_)),%delete old board,
	asserta(board(TerrainMap,FinBF,NewKhan)),!.

% resurrectionTarget(BF,Side,TargetIndex)
resurrectionTarget(BF,0,TargetIndex):-
	sublistOf(1,5,BF,Side0), indexOf(Side0,44,TargetIndex).
resurrectionTarget(BF,1,TargetIndex):-
	sublistOf(7,11,BF,Side1),indexOf(Side1,44,TargetIndex).

resurrectionPosition(BF,TerrainMap,KHAN,GardenTomb):-
	setof(
		Index,
		nth0(Index,TerrainMap,KHAN),
		Tmp
	),
	subtract(Tmp,BF,GardenTomb).

tryResurrect(TerrainMap,Dest,KHAN):-
	nth0(Dest,TerrainMap,Terrain), KHAN == Terrain.
resurrect(Side,Dest):-
	board(TerrainMap,BF,KHAN),
	resurrectionTarget(BF,Side,TargetIndex),
	tryResurrect(TerrainMap,Dest,KHAN),
	modifyList2(TargetIndex,Dest,BF,FinBF,0),
	retract(board(_,BF,_)),%delete old board,
	asserta(board(TerrainMap,FinBF,KHAN)),!.

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
