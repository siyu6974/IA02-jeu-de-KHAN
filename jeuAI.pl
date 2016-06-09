% ----------------------------AI-------------------------------
minimax(Side,0,Max,Val,_):-
	board(_,BF,_),
	SideAbs is (Side+Max+1) mod 2,
	evaluate(SideAbs,BF,Val,0),!.
minimax(Side,_,1,BestVal,_):-
	gameOver(Side),
	BestVal is -1000,!.
minimax(Side,_,0,BestVal,_):-
	gameOver(Side),
	BestVal is 1000,!.
% minimax(Side,Depth,1,Val,BestMove) SideAbs === initial Side
minimax(Side,Depth,Max,BestVal,BestMove):-
	allPossibleMoves(Side,PossibleMoves),
	findall(
		[RetrievedVal,APossibleMove],
		(
			member(APossibleMove,PossibleMoves),
			board(TerrainMap,BF,KHAN),
			nth(1,APossibleMove,From),nth(2,APossibleMove,To),
			move(From,Side,To),
			Op is (Side+1) mod 2,
			Min is (Max+1) mod 2,
			Deeper is Depth - 1,
			minimax(Op,Deeper,Min,RetrievedVal,_),
			retractall(board(_,_,_)),asserta(board(TerrainMap,BF,KHAN)) %clone
		),
		ValMovePairs
	),
	regroup(ValMovePairs,Vals,Moves),
	(
		Max == 1,
		max_list(Vals,BestVal),
		indexOf(Vals,BestVal,MaxValInx),
		nth0(MaxValInx,Moves,BestMove)
		;
		min_list(Vals,BestVal),
		indexOf(Vals,BestVal,MinValInx),
		nth0(MinValInx,Moves,BestMove)
	),!.

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
	countPawns(Camarades,Vpawns),mobilityScore(Side,Vmob),
	VSelf is Vq+Vpawns+Vmob,
	Opponent is (Side+1) mod 2,
	(
		Flag == 1,
		Val is VSelf
	;
		evaluate(Opponent,BF,ValOp,1),
		Val is VSelf - ValOp
	),!.

% Part of materialScore
countPawns([],0).
countPawns([Camarade|Rest],Return):-
	countPawns(Rest,V),
	(
		Camarade < 44,
		Return is V+5
	;
	Return is V
	),!.

mobilityScore(Side,Return):-
	allPossibleMoves(Side,AllPossibleMoves),
	length(AllPossibleMoves,Len),
	Return is Len*0.2.

aiInitBoard(AiOpening):-
	openingLib(OpeningLib),
	choose(OpeningLib,AiOpening).
aiReact(BestAiReact):-
	board(T,BF,_),
	slice(1,6,BF,OpPieces),
	reactLib(ReactLib),
	findall(
		[AiReact,V],
		(
			member(AiReact,ReactLib),
			append(OpPieces,AiReact,InitBF),
			retract(board(_,_,_)),%delete old empty board,
			asserta(board(T,InitBF,0)),
			minimax(0,3,0,V,_)
		),
		DB
	),
	regroup(DB,AiReacts,Vs),
	max_list(Vs,BestVal),
	indexOf(Vs,BestVal,MaxValInx),
	nth0(MaxValInx,AiReacts,BestAiReact).
