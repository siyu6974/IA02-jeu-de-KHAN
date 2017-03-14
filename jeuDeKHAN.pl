% Projet IA02
:- include('movementRules.pl').
:- include('jeuDeKHAN_lib.pl').
:- include('translatedic.pl').
:- include('opening.pl').
:- include('jeuAI.pl').
:- dynamic(board/3).
terrainMap([2,3,1,2,2,3,2,1,3,1,3,1,1,3,2,3,1,2,3,1,2,1,3,2,2,3,1,3,1,3,2,1,3,2,2,1]).

printBattleField(-1,0,BF) :-
	nl, write('  A B C D E F'),  printLineNumber(0,BF),!.
printBattleField(Line,6,BF) :-
	NextLine is Line+1, printLineNumber(NextLine,BF).
printBattleField(Line,Row,BF) :-
	Scanning is Line*6+Row,
	(
		slice(1,5,BF,Side0),member(Scanning,Side0), write('o ')
	;
		slice(7,11,BF,Side1),member(Scanning,Side1), write('x ')
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
	board(TerrainMap,BF,KHAN),write(' '),printMatrix(TerrainMap,6),
	nl, printBattleField(-1,0,BF),nl,write('   '),
	printDeadPiece(BF),!,nl,write('KHAN = '),write(KHAN),nl,nl.

% for debug only
initBoard :-
	terrainMap(TerrainMap),
    % asserta(board(TerrainMap,[44,44,44,44,14,11, 44, 44, 44, 44, 1, 29],1)).
	asserta(board(TerrainMap,[44,44,44,5,13,11, 44, 44, 44, 44, 44, 29],2)).

% -------------------------------Game Rule.------------------------------------
% allPossibleMoves(Side,Result)
allPossibleMoves(Side,AllPossibleMoves):-
	board(TerrainMap,BF,KHAN),
	(
		Side == 0,slice(1,6,BF,Camarades)
	;
		slice(7,12,BF,Camarades)
	),!,
	findall(
		[Piece|PossibleMove],
		(member(Piece,Camarades),tryMove(Piece,TerrainMap,BF,KHAN,PossibleMove)),
		R1
	),
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
	),distributer(Result,AllPossibleMoves),!.
% tryMove prevents friendly fire
tryMove(Pos,TerrainMap,BF,KHAN,Result):-
		Pos<36, nth0(Pos,TerrainMap,Step),%obeying terrainMap
		(
			KHAN == Step
		;
			KHAN == 0
		),!,
		possMove(Step,Pos,Tmp),indexOf(BF,Pos,N),
		(
			N=<5,%side 0
			slice(1,6,BF,Side0),subtract(Tmp,Side0,Result)
		;
			N>5,%side 1
			slice(7,12,BF,Side1),subtract(Tmp,Side1,Result)
		),!.

% move shows the consiquence of a move
move(44,Side,Dest) :-
	resurrect(Side,Dest),!.
move(Pos,Side,Dest) :-
	board(TerrainMap,BF,_),
	(
		Side==0,%side 0 moving
		slice(7,12,BF,Side1),member(Dest,Side1),
		modifyList(Dest,44,BF,NewBF), modifyList(Pos,Dest,NewBF,FinBF)%capture
	;
		Side==1,%side 1 moving
		slice(1,6,BF,Side0),member(Dest,Side0),
		modifyList(Dest,44,BF,NewBF), modifyList(Pos,Dest,NewBF,FinBF)%capture
	;
		modifyList(Pos,Dest,BF,FinBF)%peace
	),
	nth0(Dest,TerrainMap,NewKhan),
	retract(board(_,BF,_)),%delete old board,
	asserta(board(TerrainMap,FinBF,NewKhan)),!.

% resurrectionTarget(BF,Side,TargetIndex)
resurrectionTarget(BF,0,TargetIndex):-
	slice(1,5,BF,Side0), indexOf(Side0,44,TargetIndex).
resurrectionTarget(BF,1,TargetIndex):-
	slice(7,11,BF,Side1),indexOf(Side1,44,TargetI),TargetIndex is TargetI+6.

resurrectionPosition(BF,TerrainMap,KHAN,GardenTomb):-
	setof(
		Index,
		nth0(Index,TerrainMap,KHAN),
		Tmp
	),
	subtract(Tmp,BF,GardenTomb).

% tryResurrect: wraps resurrectionTarget and resurrectionPosition
tryResurrect(Side,GardenTomb):-
	board(T,BF,KHAN),
	resurrectionTarget(BF,Side,_),
	resurrectionPosition(BF,T,KHAN,GardenTomb).
resurrect(Side,Dest):-
	board(TerrainMap,BF,KHAN),
	resurrectionTarget(BF,Side,TargetIndex),
	modifyList2(TargetIndex,Dest,BF,FinBF,0),
	retract(board(_,BF,_)),%delete old board,
	asserta(board(TerrainMap,FinBF,KHAN)),!.

gameOver(0):-
	board(_,BF,_),nth(6,BF,Q),Q==44.
gameOver(1):-
	board(_,BF,_),nth(12,BF,Q),Q==44.

% -------------------------------UI/UE-------------------------------------------
play :-
    nl,retractall(board(_,_,_)),
    write('======================================'), nl,
	write('========== Prolog Jeu de KHAN ========'), nl,
	write('======================================'), nl,
	write('Creation of Siyu ZHANG & Mengjia SUI'), nl,nl,
	choosemode(Mode),
	(
		Mode =:= 1, difficultity(Level),
		playerChooseColor(HumainSide),boardPreparation(HumainSide),
		start(0,HumainSide,0,Level)
	;
		Mode =:= 2,
		boardPreparation(2),
		start(0,_,1,_)
	;
		Mode =:= 3,
		aiInitBoard(AiOpening),
		choose([s1,s2,s3,s4],RandS),
		translate(RandS,S),
		append(AiOpening,[44,44,44,44,44,44],AfterOpening),
		asserta(board(S,AfterOpening,0)),
		aiReact(BestAiReact),
		append(AiOpening,BestAiReact,InitBF),
		retractall(board(_,_,_)),
		asserta(board(S,InitBF,0)),
 	% absence of humain player, must never equal to SideToPlay, thus 3
		start(0,3,0,3)
	),!.
%	start(SideToPlay,HumainSide,Pvp,DifficultityLevel) Pvp = 1 if pvp, 0 if not
choosemode(Mode):-
	write('Choose the mode of play:' ), nl,
	write('1. Human VS Computer'),nl,
	write('2. Human VS Human'),nl,
	write('3. Computer VS Computer'),nl,read(Mode).
start(SideToPlay,_,1,_):-
	Op is (SideToPlay+1) mod 2,
	userMove(SideToPlay),
	afficherBoard,
	(
		\+ gameOver(0), \+gameOver(1),start(Op,_,1,_)
	;
		nl,write('GAMEOVER'),retractall(board(_,_,_))
	),!.
start(SideToPlay,HumainSide,0,L):-
	Op is (SideToPlay+1) mod 2,
	(
		SideToPlay == HumainSide,
		userMove(SideToPlay)
	;
		minimax(SideToPlay,L,1,_,BestMove),
		nth(1,BestMove,From),nth(2,BestMove,To),
		move(From,SideToPlay,To)
	),!,afficherBoard,
	(
		\+ gameOver(0), \+gameOver(1),start(Op,HumainSide,0,L)
	;
		nl,write('GAMEOVER'),retractall(board(_,_,_))
	),!.
difficultity(Level):-
	write('Choose difficultity level. Type easy, normal, hard or challenge.'),
	nl, write('( challenge mode requires lots of computing power, don\'t try this on a laptop!)'),
	nl,read(Difficultity),
	translateD(Difficultity,Level).
%playerChooseColor
% Ask the color for the human player and start the game with it.
playerChooseColor(HumainSide) :-
	nl, write('Side for human player ? ("o" for first and "x" for second)'), nl,
	read(Player), nl,
	(
		Player == o,
		HumainSide = 0
	;
		Player == x,
		HumainSide = 1
	;
	    % If not x or o -> not a valid color
		write('Error : This is not a valid side !'), nl,
		playerChooseColor(HumainSide)      % Ask again
	),!.

placementValid(Order,R,R1,R2,R3,R4,R5):-
	(
		Order =:= 0, %humain moves first
		member(R,[0,1,2,3,4,5,6,7,8,9,10,11]),
		member(R1,[0,1,2,3,4,5,6,7,8,9,10,11]),
		member(R2,[0,1,2,3,4,5,6,7,8,9,10,11]),
		member(R3,[0,1,2,3,4,5,6,7,8,9,10,11]),
		member(R4,[0,1,2,3,4,5,6,7,8,9,10,11]),
		member(R5,[0,1,2,3,4,5,6,7,8,9,10,11]),
		write('OK'),nl % true if placed in first two lines
	;
		Order =:= 1, % move second
		member(R,[35,34,33,32,31,30,29,28,27,26,25,24]),
		member(R1,[35,34,33,32,31,30,29,28,27,26,25,24]),
		member(R2,[35,34,33,32,31,30,29,28,27,26,25,24]),
		member(R3,[35,34,33,32,31,30,29,28,27,26,25,24]),
		member(R4,[35,34,33,32,31,30,29,28,27,26,25,24]),
		member(R5,[35,34,33,32,31,30,29,28,27,26,25,24]),
		write('OK'),nl % true if placed in last two lines
	;
		write('Nope, can\'t place your pieces like that'),nl,
		write('Entre "play." to restart!'),nl,
		fail
	),!.
% boardPreparation(mode) 0 humain first, 1 cpu first, 2 pvp
boardPreparation(0):-
	terrainMap(TerrainMap),
	asserta(board(TerrainMap,[44,44,44,44,44,44,44, 44, 44, 44, 44, 44],0)),
	nl, afficherBoard, nl,
	write('Which side do you want? s1.Top s2.Buttom s3.Left s4.Right'),nl,
	read(Side),nl,write('Here is the new gameboard:'),translate(Side,S),
	retractall(board(_,_,_)),
	asserta(board(S,[44,44,44,44,44,44,44, 44, 44, 44, 44, 44],0)),
	playerInitBoard(R,R1,R2,R3,R4,R5),
	placementValid(0,R,R1,R2,R3,R4,R5),
	write('UserInitBoard Finished'), nl,
	retract(board(_,_,_)),%delete old empty board,
	asserta(board(TerrainMap,[R1,R2,R3,R4,R5,R,33, 35, 28, 20, 30, 13],0)),
	aiReact(BestAiReact),
	append([R1,R2,R3,R4,R5,R],BestAiReact,InitBF),
	retract(board(_,_,_)),%delete old empty board,
	asserta(board(S,InitBF,0)),write('AI initBoard completed'),
	nl, afficherBoard.
boardPreparation(1):-
		aiInitBoard(AiOpening),
		choose([s1,s2,s3,s4],RandS),
		translate(RandS,S),
		append(AiOpening,[44,44,44,44,44,44],AfterOpening),
		asserta(board(S,AfterOpening,0)),
		playerInitBoard(R,R1,R2,R3,R4,R5),
		placementValid(1,R,R1,R2,R3,R4,R5),
		write('UserInitBoard Finished'), nl,
		append(AiOpening,[R1,R2,R3,R4,R5,R],InitBF),
		retract(board(_,_,_)),%delete old empty board,
		asserta(board(S,InitBF,0)), nl,afficherBoard.
boardPreparation(2):-
	terrainMap(TerrainMap),
	asserta(board(TerrainMap,[44,44,44,44,44,44,44, 44, 44, 44, 44, 44],0)),
	nl, afficherBoard, nl,
	write('Which side do you want? s1.Up s2.Down s3.Left s4.Right'),nl,
	read(Side),nl,write('Here is the new gameboard:'),translate(Side,NewTerrainMap),
	retractall(board(_,_,_)),
	asserta(board(NewTerrainMap,[44,44,44,44,44,44,44, 44, 44, 44, 44, 44],0)),
	playerInitBoard(R,R1,R2,R3,R4,R5),
	placementValid(0,R,R1,R2,R3,R4,R5),
	retractall(board(_,_,_)),
	asserta(board(NewTerrainMap,[R,R1,R2,R3,R4,R5,44, 44, 44, 44, 44, 44],0)),
	write('User 1 InitBoard Finished'), nl,
	write('User 2 InitBoard'), nl,
	playerInitBoard(S,S1,S2,S3,S4,S5),
	placementValid(1,S,S1,S2,S3,S4,S5),
	write('User 2 InitBoard Finished'),nl,
	append([R1,R2,R3,R4,R5,R],[S,S1,S2,S3,S4,S5],InitBF),
	retract(board(_,_,_)),%delete old empty board,
	asserta(board(NewTerrainMap,InitBF,0)),write('AI initBoard completed'),
	nl, afficherBoard.

playerInitBoard(R,R1,R2,R3,R4,R5):-
	nl, afficherBoard, nl,
	write('Position for Queen, place in the line 0/1 if you move first, line 4/5 if not'), nl,
	read(Reine), nl, translate(Reine,R),
	write('Positions for Pawn_1, place in the line 0/1 if you move first, line 4/5 if not'), nl,
	read(S1), nl, translate(S1,R1),
	write('Positions for Pawn_2, place in the line 0/1 if you move first, line 4/5 if not'), nl,
	read(S2), nl, translate(S2,R2),
	write('Positions for Pawn_3, place in the line 0/1 if you move first, line 4/5 if not'), nl,
	read(S3), nl, translate(S3,R3),
	write('Positions for Pawn_4, place in the line 0/1 if you move first, line 4/5 if not'), nl,
	read(S4), nl, translate(S4,R4),
	write('Positions for Pawn_5, place in the line 0/1 if you move first, line 4/5 if not'), nl,
	read(S5), nl, translate(S5,R5).

generateHint(AllPossibleMoves,Hints):-
	setof(
		Hint,
		MoveablePiece^FDPair^(
			member(FDPair,AllPossibleMoves),
			nth(1,FDPair,MoveablePiece),
			translate(Hint,MoveablePiece) %machine readable->humain readable
		),
		Hints
	).
% User Move
userMove(Side):-
	userMoveFrom(Side,From),
	userMoveTo(Side,From).

userMoveFrom(Side,Position):-
	allPossibleMoves(Side,AllPossibleMoves),
	generateHint(AllPossibleMoves,Hints),
	nl, write('It\'s your turn !'), nl,
  write('Which one would you want to move ? 44 for resurrection'), nl,
	write('Hint:  '), write(Hints),nl,
	read(Pos),nl, translate(Pos, Position),
	board(_,BF,_),
	(
		Position =:= 44,
		resurrectionTarget(BF,Side,_) %true if can resurrect
	;
		%nothing to resurrect or moving normally
		allPossibleMoves(Side,AllPossibleMoves),
		regroup(AllPossibleMoves,CouldMove,_),
		member(Position,CouldMove)% true if Pos can move
	;
		write('Nope, can\'t move that piece'),nl,
		userMoveFrom(Side,_) % retry
	),!.

userMoveTo(Side,Position):-
	board(T,BF,_),
	write('Where would you like to put it ?'),nl,
	read(Dest),nl, translate(Dest, Destination),
	(
		tryMove(Position,T,BF,0,CouldGo), % KHAN is 0, since we are sure that piece can move
		member(Destination,CouldGo),
		move(Position,Side,Destination)
	;
		Position =:= 44,
		tryResurrect(Side,GardenTomb),
		member(Destination,GardenTomb),
		resurrect(Side,Destination)
	;
		write('Can\'t do that'),nl,
		userMoveTo(Side,Position)
	),!.
