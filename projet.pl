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
	printDeadPiece(BF),!,nl,write('KHAN = '),write(KHAN).


initBoard :-
	terrainMap(TerrainMap),
    asserta(board(TerrainMap,[0, 1, 3, 44, 44, 4, 16, 44, 17, 10, 5, 44],1)).

% allPossibleMove(Side,Result)
allPossibleMove(Side,AllPossibleMoves):-
	board(TerrainMap,BF,KHAN),
	(
		Side == 0,slice(1,6,BF,Camarades)
	;
		slice(7,12,BF,Camarades)
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
	),distributer(Result,AllPossibleMoves),!.
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
		slice(1,6,BF,Side0),subtract(Tmp,Side0,Result)
	;
		N>5,%side 1
		slice(7,12,BF,Side1),subtract(Tmp,Side1,Result)
	),!.

% move gives the consiquence of a move
move(Pos,Dest) :-
	board(TerrainMap,BF,_), indexOf(BF,Pos,N),
	(
		N=<5,%side 0 moving
		slice(7,12,BF,Side1),member(Dest,Side1),
		modifyList(Dest,44,BF,NewBF), modifyList(Pos,Dest,NewBF,FinBF)%capture
	;
		N>5,%side 1 moving
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
	slice(7,11,BF,Side1),indexOf(Side1,44,TargetIndex).

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

minimax(Side,0,Max,Val,_):-

	board(_,BF,_),
	SideAbs is (Side+Max+1) mod 2,
	evaluate(SideAbs,BF,Val,0).
% board(T,BF,K), Board = [T,BF,K],nth(1,Board,TerrainMap),nth(2,Board,BF),nth(3,Board,KHAN),
% minimax(Side,3,1,Val,BestMove)
minimax(Side,Depth,Max,Val,BestMove):-
	allPossibleMove(Side,PossibleMoves),
	member(APossibleMove,PossibleMoves),
	board(TerrainMap,BF,KHAN),
	asserta(board(TerrainMap,BF,KHAN)),%Clone
	nth(1,APossibleMove,From),nth(2,APossibleMove,To),
	move(From,To),
	Min is (Max+1) mod 2,
	Deeper is Depth -1,
	Op is (Side+1) mod 2,
	minimax(Op,Deeper,Min,Val,BestMove).


% evaluate(Side,BF,Val,0)
evaluate(Side,BF,Val,Flag):-
	(
		Side == 0,slice(1,6,BF,Camarades)
	;
		slice(7,12,BF,Camarades)
	),!,
	nth(6,Camarades,Q),
	(
		Q < 44,
		Vq is 995 %Queen is worth actually 1000, of which 5 from countPawns
	;
		Vq is 0
	),!,
	countPawns(Camarades,Vpawns),
	VSelf is Vq+Vpawns,
	Opponent is (Side+1) mod 2,
	(
		Flag == 1,
		Val is VSelf
	;
		evaluate(Opponent,BF,ValOp,1),
		Val is VSelf - ValOp
	),!.

countPawns([],0).
countPawns([Camarade|Rest],Return):-
	countPawns(Rest,V),
	(
		Camarade < 44,
		Return is V+5
	;
	Return is V
	),!.

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
	write('======== Prolog Jeu de KHAN ======='), nl,
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
		asserta(board(TerrainMap,[44,44,44,44,44,44,44, 44, 44, 44, 44, 44],0)),
		nl, afficherBoard, nl,

		write('Position for Queen, position from a0,a1 to f4,f5'), nl,
	    read(Reine), nl, write('OK Reine'), nl, translate(Reine,R),
		write('Positions for Pawn_1, position from a0 to f5'), nl,
		read(S1), nl, write('OK Pawn_1'), nl, translate(S1,R1),
		write('Positions for Pawn_1, position from a0 to f5'), nl,
		read(S2), nl, write('OK Pawn_2'), nl, translate(S2,R2),
		write('Positions for Pawn_3, position from a0 to f5'), nl,
		read(S3), nl, write('OK Pawn_3'), nl, translate(S3,R3),
		write('Positions for Pawn_4, position from a0 to f5'), nl,
		read(S4), nl, write('OK Pawn_4'), nl, translate(S4,R4),
		write('Positions for Pawn_4, position from a0 to f5'), nl,
		read(S5), nl, write('OK Pawn_5'), nl, translate(S5,R5),
	    write('UserInitBoard Finish'), nl,
		% valeurKhan(R,[2,3,1,2,2,3,2,1,3,1,3,1,1,3,2,3,1,2,3,1,2,1,3,2,2,3,1,3,1,3,2,1,3,2,2,1],K),
		(Player = o,
			asserta(board(TerrainMap,[R1,R2,R3,R4,R5,R,44, 44, 44, 44, 44, 44],0)), nl, afficherBoard
		 ;
		 Player = x,
			asserta(board(TerrainMap,[44, 44, 44, 44, 44, 44,R1,R2,R3,R4,R5,R],0)), nl, afficherBoard
		)
		%play([x, play, EmptyBoard], Player)
	  ).
 % User Move
 userMove:- nl, write('It\'s your turn !'), nl,
			    write('Which one would you want to move ?'), nl,
				read(Pos),nl, translate(Pos, Position),
				write('Where would you like to put it ?'),nl,
				read(Dest),nl, translate(Dest, Destination),
				\+ move(Position,Destination),
				write('Move invalid!'), nl,
				userMove
				;
				move(Position,Destination).

% Decide first KHAN
valeurKhan(1,[T|_],T):- !.
valeurKhan(R,[_|Q],Res):- R2 is R-1, valeurKhan(R2,Q,Res).
