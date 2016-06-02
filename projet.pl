% Projet IA02
:- include('movementRules.pl').
:- include('jeuDeKHAN_lib.pl').
:- include('translatedic.pl').
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
		Side == 0,sublistOf(1,6,BF,Camarades)
	;
		sublistOf(7,12,BF,Camarades)
	),!,
	findall(
		[Piece|PossibleMove],
		(member(Piece,Camarades),tryMove(Piece,TerrainMap,BF,KHAN,PossibleMove)),
		R1),
	(
		length(R1,Len),
		%KHAN sucks
		(
			Len==0%KHAN sucks because of terrain
		;
			lenTotal(R1,Len2),%KHAN sucks because of terrain+blocage
			Len==Len2			%eg: R1=[[17]], 17 could move but blocked
		),!,
		(
			resurrectionTarget(BF,Side,_),
			resurrectionPosition(BF,TerrainMap,KHAN,GardenTomb),
			R2 = [[44|GardenTomb]]
		;
			R2 =[]
		),!,
		findall(
			[Piece|PossibleMove],
			(member(Piece,Camarades),tryMove(Piece,TerrainMap,BF,0,PossibleMove)),
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


play :-
    nl,
    write('==================================='), nl,
	write('============ Prolog KHAN =========='), nl,
	write('==================================='), nl, nl,
	write('@Right of Siyu ZHANG & Mengjia SUI'), nl,
	playAskColor.

%playAskColor
% Ask the color for the human player and start the game with it.
playAskColor :-
	  nl, write('Side for human player ? ("o" for first and "x" for second)'), nl,
	  read(Player), nl,
	  (
	    Player \= o, Player \= x, !,    % If not x or o -> not a valid color
	    write('Error : This is not a valid side !'), nl,
	    playAskColor                     % Ask again
	    ;
		terrainMap(TerrainMap),
		asserta(board(TerrainMap,['','','','','','','', '', '', '', '', ''],?)),
		nl, afficherBoard, nl,
		write('Position for Reine, position from a0,a1 to f4,f5'), nl,
	    read(Reine), nl, write('OK Reine'), nl, translate(Reine,R),
		write('Positions for Sbire_1, position from a0 to f5'), nl,
		read(S1), nl, write('OK Sbire_1'), nl, translate(S1,R1),
		write('Positions for Sbire_1, position from a0 to f5'), nl,
		read(S2), nl, write('OK Sbire_2'), nl, translate(S2,R2),
		write('Positions for Sbire_3, position from a0 to f5'), nl,
		read(S3), nl, write('OK Sbire_3'), nl, translate(S3,R3),
		write('Positions for Sbire_4, position from a0 to f5'), nl,
		read(S4), nl, write('OK Sbire_4'), nl, translate(S4,R4),
		write('Positions for Sbire_4, position from a0 to f5'), nl,
		read(S5), nl, write('OK Sbire_5'), nl, translate(S5,R5),
		
	    write('UserInitBoard Finish'), nl,
		(Player = o,
			asserta(board(TerrainMap,[R1,R2,R3,R4,R5,R,'', '', '', '', '', ''],?)), nl, afficherBoard
		 ;
		 Player = x,
			asserta(board(TerrainMap,['', '', '', '', '', '',R1,R2,R3,R4,R5,R],?)), nl, afficherBoard
		)
		%play([x, play, EmptyBoard], Player)
	  ).

 % User Move
 userMove:- nl, write('It\'s your turn !'), nl,
			    write('Which one would you want to move ?'), nl,
				read(Pos),nl, translate(Pos, Position),
				write('Where would you like to put it ?'),nl,
				read(Dest),nl, translate(Dest, Destination),
				% not(move(Position,Destination)),
				% userMove
				%;
				move(Position,Destination).
