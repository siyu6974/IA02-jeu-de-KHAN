# 2 3 1 2 2 3
# 2,1,3,1,3,1
# 1,3,2,3,1,2
# 3,1,2,1,3,2
#,2,3,1,3,1,3
# 2,1,3,2,2,1
from __future__ import print_function
import random
from Tkinter import *
from time import clock
class Board:
    terrainMap = [2,3,1,2,2,3,2,1,3,1,3,1,1,3,2,3,1,2,3,1,2,1,3,2,2,3,1,3,1,3,2,1,3,2,2,1]
    Khan = 0
    piecePos = [] # P0..P4,Q1,P6..P10,Q2 died=44
    def __init__(self,battleField=None,Khan=None):
        if battleField is None:
            for i in range(0,12):
                self.piecePos.append(36)
        else:
            self.piecePos = list(battleField)
        if Khan is not None:
            self.Khan = Khan
    def printPiecePos(self):
        for i in range(0,12):
            print(convertCoord(self.piecePos[i]), end=" ")
            if i == 5:
                print()
        print()

def convertCoordAlphaNum(n):
    strr = chr(n%6+97)
    strr += str(n // 6)
    return strr
def convertCoord(n):
    return n//6,n%6
def rotateProjection(coor,n):
    if n==0:
        return coor
    (x,y) = convertCoord(coor)
    return rotateProjection(y*6+6-1-x,n-1)
def initBoard(board):
    face = random.randint(0, 3)
    for i in range(0,6):
        tmp = rotateProjection(random.randint(0, 11), face)
        while tmp in board.piecePos:
            tmp = rotateProjection(random.randint(0, 11), face)
        board.piecePos[i] = tmp
    for i in range(6,12):
        tmp = rotateProjection(random.randint(0, 11), (face+2)%4)
        while tmp in board.piecePos:
            tmp = rotateProjection(random.randint(0, 11), (face+2)%4)
        board.piecePos[i] = tmp
def printBoard(gameBoard):
    print("KHAN = ", gameBoard.Khan)
    print("                  A B C D E F")
    for i in range(0,6):
        for j in range(0,6):
            tmp = i*6+j
            print(gameBoard.terrainMap[tmp],end=" ")
        print("   ",end="")
        print(i,end="  ")
        for j in range(0,6):
            piece = i*6+j
            if piece in set(gameBoard.piecePos[0:5]):
                print("o",end=" ")
            elif piece == gameBoard.piecePos[5]:
                print("O",end=" ")
            elif piece in set(gameBoard.piecePos[6:11]):
                print("x",end=" ")
            elif piece == gameBoard.piecePos[11]:
                print("X",end=" ")
            else:
                print('_',end=" ")
        print("")
    for piece in gameBoard.piecePos[0:5]:
        if piece == 44:
            print("o ",end=" ")
    print("   ", end=" ")
    for piece in gameBoard.piecePos[6:11]:
        if piece == 44:
            print("x ", end=" ")
    print();

def up(n):
    tmp = n-6
    if tmp>0:
        return tmp
    return 40
def down(n):
    tmp = n+6
    if tmp<36:
        return tmp
    return 40
def left(n):
    tmp = n-1
    if tmp//6 == n //6:
        return tmp
    return 40
def right(n):
    tmp = n+1
    if tmp//6 == n //6:
        return tmp
    return 40

def basicMove(n, direction):
    switcher = {
        0: up,
        1: right,
        2: down,
        3: left,
    }
    func = switcher.get(direction)
    return func(n)

def possibleMove(pos, board):
    side = board.piecePos.index(pos) // 6
    destinations = [[pos,[]]]
    steps = board.terrainMap[pos]
    # print(steps)
    possibleDirections = [0,1,2,3]
    popCounter = 1
    i = 0
    while i != popCounter:
        candidate = destinations.pop(0)
        i += 1
        oldPos = candidate[0]
        history = list(candidate[1])
        history.append(oldPos)
        for aDirection in possibleDirections:
            newPos = basicMove(oldPos, aDirection)
            if len(history) < steps and newPos in board.piecePos: #no blockage
                continue
            if newPos != 40 and newPos not in history: #no turning back
                destinations.append([newPos,history])
                if len(history) < steps:
                    popCounter += 1 # one more intermidiaire step
    destinationsCopy = list(destinations)
    for (endPoint,_) in destinations:
        if endPoint in board.piecePos[side*6:(side+1)*6]: # no friendly fire, never betray your queen
            destinationsCopy.remove([endPoint,_])
    if len(destinations)!=0:
        Tmp = [x[0] for x in destinationsCopy]
        return Tmp
    return False
    # destinationConverted = []
    # for (endPoint,_) in destinations:
    #     destinationConverted.append(convertCoord(endPoint))
    # return destinationConverted

def _movablePiece(board, side): #  judgement only by KHAN, can be wrong
    movablePieces = []
    KHANsucks = False
    for piece in board.piecePos[side*6:(side+1)*6]:
        if piece!=44 and board.terrainMap[piece]==board.Khan:
            movablePieces.append(piece)
    if len(movablePieces)==0: # not obeying KHAN
        KHANsucks = True
        movablePieces = list(board.piecePos[side*6:(side+1)*6])
    return movablePieces,KHANsucks

def allPossibleMove(board,side):
    allPossibleMoves = []
    (MovablePieces, KHANsucks) = _movablePiece(board,side)
    while True:
        if KHANsucks:
            possibleResurrectTargets=possibleResurrectTarget(board, side)
            if len(possibleResurrectTargets):
                for aPossibleResurrectionPos in possibleResurrectionPostion(board):
                    allPossibleMoves.append((44, aPossibleResurrectionPos))
        for aMovablePiece in MovablePieces:
            if aMovablePiece != 44:
                posibleMoves = possibleMove(aMovablePiece, board)
                if posibleMoves: # really movable
                    for aPossibleMove in posibleMoves:
                        allPossibleMoves.append((aMovablePiece, aPossibleMove))
        if len(allPossibleMoves) == 0:  # not obeying KHAN because of blockage
            KHANsucks = True
            MovablePieces = list(board.piecePos[side * 6:(side + 1) * 6])
        else:
            break
    return list(set(allPossibleMoves))

def possibleResurrectTarget(board,side):
    possibleResurrectTargets = []
    for piece in board.piecePos[side*6:(side+1)*6-1]:#queen cant be reanimate
        if piece == 44:
            possibleResurrectTargets.append(piece)
            break # only need one
    return possibleResurrectTargets
def possibleResurrectionPostion(board):
    possibleResurrectionPostions = set(range(0,36))
    possibleResurrectionPostions = possibleResurrectionPostions.difference(set(board.piecePos))
    possibleResurrectionPostions = list(possibleResurrectionPostions)
    possibleResurrectionPostions = filter(lambda piece: board.terrainMap[piece]==board.Khan,
                                          possibleResurrectionPostions)
    return possibleResurrectionPostions

def move(pieceToMove,dest,board, side, check=False):
    #resurrection
    if pieceToMove == 44:
        possibleResurrectTargets = possibleResurrectTarget(board, side)
        if len(possibleResurrectTargets):
            chosenOne = possibleResurrectTargets.pop(0)
            if dest not in possibleResurrectionPostion(board):
                print("Resurrection cant be done")
                return False
            board.piecePos[board.piecePos.index(chosenOne)] = dest
            #printBoard(board)
            return True
    #normal move
    if check:
        if pieceToMove not in board.piecePos:
            print("No piece there")
            return False
        sideInputed = board.piecePos.index(pieceToMove) // 6
        if sideInputed != side:
            print(pieceToMove, "Not yours to play")
            return False
        reallyMovablePieces = [x[0] for x in allPossibleMove(board, side)]
        if pieceToMove not in reallyMovablePieces:
            print("This piece can not be moved")
            return False
        possibleDest = possibleMove(pieceToMove,board)
        if dest not in possibleDest:
            print("can not move like that")
            return False
    # capture check
    for piece in board.piecePos:
        if piece == dest:
            board.piecePos[board.piecePos.index(piece)]= 44
    board.piecePos[board.piecePos.index(pieceToMove)] = dest
    board.Khan = board.terrainMap[dest]
    # printBoard(board)
    return True

def moveInterpreter(usrInput):
    usrInput = usrInput.lower()
    #  resurrection
    if len(usrInput)==2:
        dest = ord(usrInput[0]) - 97 + int(usrInput[1]) * 6
        return 44,dest
    #  normal move
    try:
        pieceToMove = ord(usrInput[0])-97+int(usrInput[1])*6
        dest = ord(usrInput[2])-97+int(usrInput[3])*6
    except TypeError or ValueError:
        return -1,-1
    return pieceToMove,dest

def userMove(board,side):
    flag = False
    hint = [convertCoordAlphaNum(x) for x in _movablePiece(board,side)[0]]
    while not flag:
        command = raw_input("next? "+', '.join(hint)+' ')
        (pieceToMove,dest)=moveInterpreter(command)
        if (pieceToMove,dest)!=(-1,-1) :
            flag = move(pieceToMove, dest, board, side, True)

def materialScore(board,side):
    matS = 0
    for piece in board.piecePos[side*6:(side+1)*6-1]:
        if piece != 44:
            matS += 4
    if board.piecePos[(side+1)*6-1] != 44: # Queen alive
        matS += 1000
    return matS

def mobilityScore(board,side):
    mobS = 0
    mobS += len(allPossibleMove(board,side))
    return mobS

def evaluate(board,side):
    otherSide = (side+1) % 2
    # return 0.2*(mobilityScore(board,side)-mobilityScore(board,otherSide))\
    #        +materialScore(board,side)-materialScore(board,otherSide)
    return materialScore(board, side) - materialScore(board, otherSide)
def evaluate2(board,side):
    otherSide = (side+1) % 2
    diffMatS = materialScore(board,side)-materialScore(board,otherSide)
    diffMobS = mobilityScore(board,side)-mobilityScore(board,otherSide)
    return diffMatS+0.2*diffMobS

def minimax(node, depth, maximizingPlayer,board,side,evalFunc=evaluate):
    newSide = (side+1) % 2
    if depth == 0:
        return evalFunc(board,(int(not maximizingPlayer)+side)%2)
    if maximizingPlayer:
        bestValue = -10000
        for child in node:
            clone = Board(board.piecePos, board.Khan)
            move(child[0],child[1],clone,side)
            v = minimax(allPossibleMove(clone,newSide), depth - 1, False, clone, newSide,evalFunc)
            bestValue = max(bestValue, v)
        return bestValue
    else:#    (* minimizing player *)
        bestValue = 10000
        for child in node:
            clone = Board(board.piecePos, board.Khan)
            move(child[0], child[1], clone, side)
            v = minimax(allPossibleMove(clone,newSide), depth - 1, True, clone, newSide,evalFunc)
            bestValue = min(bestValue, v)
        return bestValue

def minimaxWithAlphaBeta(node, depth, alpha, beta, maximizingPlayer,side,depthConst,evalFunc):
    newSide = (side+1) % 2
    returnMove = (36,36)
    if depth == 0:
        return evalFunc(node,(int(not maximizingPlayer)+side)%2),returnMove
    if maximizingPlayer:
        v = -10000
        aPM = allPossibleMove(node, side)
        moveSelector = []
        for aMove in aPM:
            child = Board(node.piecePos, node.Khan)
            move(aMove[0], aMove[1],child,side)
            tmp = minimaxWithAlphaBeta(child, depth - 1, alpha, beta, False, newSide,depthConst,evalFunc)
            v = max(v,tmp[0])
            alpha = max(alpha, v)
            if depth==depthConst:
                moveSelector.append(v)
            if beta<=alpha:
                break
        if depth == depthConst:
            returnMove=aPM[moveSelector.index(max(moveSelector))]
        return v, returnMove
    else:#    (* minimizing player *)
        v = 10000
        aPM = allPossibleMove(node, side)
        for aMove in aPM:
            child = Board(node.piecePos, node.Khan)
            move(aMove[0], aMove[1], child, side)
            tmp = minimaxWithAlphaBeta(child, depth - 1, alpha, beta, True, newSide, depthConst, evalFunc)
            v = min(v,tmp[0])
            beta = min(beta, v)
            if beta<=alpha:
                break
        return v, returnMove

def generateMove(board,sideToPlay,depth=3,alphaBeta=True,evalFunc=evaluate):
    moveScores = []
    if alphaBeta:
        (tmp,bestMove)=minimaxWithAlphaBeta(board,depth,-10000,10000,True,sideToPlay,depth,evalFunc)
        return bestMove
    else:
        allPossibleMoves = allPossibleMove(board, sideToPlay)
        for aPossibleMove in allPossibleMoves:
            moveScores.append(minimax([aPossibleMove],depth,True,board,sideToPlay,evalFunc))
        bestMove = allPossibleMoves[moveScores.index(max(moveScores))]
        return bestMove

def gameIsOver(board):
    for side in (0,1):
        if board.piecePos[(side + 1) * 6 - 1]==44:
            return side
    return -1

def main():
    # gameBoard = Board([6,7,8,9,10,5,22,25,27,28,29,32])
    gameBoard = Board()
    initBoard(gameBoard)
    sideToPlay = 0
    roundCount = 0
    AI1Time,AI2Time = 0,0
    while True:
        print("side to play = ", sideToPlay)
        start = clock()
        # printBoard(gameBoard)
        if sideToPlay==0:
            AImove = generateMove(gameBoard, sideToPlay, 3, True,evaluate2)
            move(AImove[0], AImove[1], gameBoard, sideToPlay)
            # userMove(gameBoard, sideToPlay)
        else:
            AImove = generateMove(gameBoard, sideToPlay,3,True,evaluate)
            move(AImove[0], AImove[1], gameBoard, sideToPlay)
            # print(AImove)

        roundCount+=1
        loser = gameIsOver(gameBoard)
        end = clock()
        # print(end - start)
        if sideToPlay==0:
            AI1Time+=end-start
        else:
            AI2Time+=end-start
        sideToPlay = (sideToPlay + 1) % 2
        if roundCount>100:
            return -1,0,0,0
        if loser!=-1:
            # printBoard(gameBoard)
            print("GAME OVER, player "+str((loser+1)%2)+" wins in "+str(roundCount)+" rounds")
            del gameBoard
            return (loser+1)%2, roundCount,AI1Time,AI2Time

main()
def AiVSAI():
    result = []
    nb = 100
    for i in range(nb):
        print('i = ',i)
        winner,roundCount,AI1Time,AI2Time=main()
        while roundCount<3:
            winner, roundCount,AI1Time,AI2Time = main()
        result.append([winner,roundCount,AI1Time,AI2Time])
        # print(AI1Time,AI2Time)
    winnerList = [x[0] for x in result]
    print(result);print()
    print("player 0 winning rate "+str(float(winnerList.count(0))/float(nb)))
    win0 = []
    win1 = []
    for tmp in result:
        if tmp[0] == 0:
            win0.append(tmp[1])
    print("player 0 win in ",float(sum(win0))/len(win0),"rounds")
    for tmp in result:
        if tmp[0] == 1:
            win1.append(tmp[1])
    print("player 1 win in ", float(sum(win1)) / len(win1),"rounds")
    print("player 0 thinking time ", sum([x[2] for x in result]))
    print("player 1 thinking time ", sum([x[3] for x in result]))

# AiVSAI()
# print("ab3 mob VS ab3")
class BoardUI(Frame):
    def __init__(self, parent):
        Frame.__init__(self, parent)
        self.parent = parent
        self.initUI()

    def initUI(self):
        self.parent.title("Calculator")

        Style().configure("TButton", padding=(0, 5, 0, 5),
                          font='serif 10')

        self.columnconfigure(0, pad=3)
        self.columnconfigure(1, pad=3)
        self.columnconfigure(2, pad=3)
        self.columnconfigure(3, pad=3)

        self.rowconfigure(0, pad=3)
        self.rowconfigure(1, pad=3)
        self.rowconfigure(2, pad=3)
        self.rowconfigure(3, pad=3)
        self.rowconfigure(4, pad=3)

        entry = Entry(self)
        entry.grid(row=0, columnspan=4, sticky=W + E)
        cls = Button(self, text="Cls")
        cls.grid(row=1, column=0)
        bck = Button(self, text="Back")
        bck.grid(row=1, column=1)
        lbl = Button(self)
        lbl.grid(row=1, column=2)
        clo = Button(self, text="Close")
        clo.grid(row=1, column=3)
        sev = Button(self, text="7")
        sev.grid(row=2, column=0)
        eig = Button(self, text="8")
        eig.grid(row=2, column=1)
        nin = Button(self, text="9")
        nin.grid(row=2, column=2)
        div = Button(self, text="/")
        div.grid(row=2, column=3)

        fou = Button(self, text="4")
        fou.grid(row=3, column=0)
        fiv = Button(self, text="5")
        fiv.grid(row=3, column=1)
        six = Button(self, text="6")
        six.grid(row=3, column=2)
        mul = Button(self, text="*")
        mul.grid(row=3, column=3)

        one = Button(self, text="1")
        one.grid(row=4, column=0)
        two = Button(self, text="2")
        two.grid(row=4, column=1)
        thr = Button(self, text="3")
        thr.grid(row=4, column=2)
        mns = Button(self, text="-")
        mns.grid(row=4, column=3)

        zer = Button(self, text="0")
        zer.grid(row=5, column=0)
        dot = Button(self, text=".")
        dot.grid(row=5, column=1)
        equ = Button(self, text="=")
        equ.grid(row=5, column=2)
        pls = Button(self, text="+")
        pls.grid(row=5, column=3)

        self.pack()


def main():
    root = Tk()
    app = Example(root)
    root.mainloop()


if __name__ == '__main__':
    main()

def clickAt(pos):
    pass
def printBoardUI(board):
    caseGroup =[]
    for i in range(6):
        for j in range(6):
            caseGroup.append(Button(text=str(board.terrainMap[i*6+j]), command=lambda: clickAt(i * 6 + j)))

def mainWithUI():
    gameBoard = Board()
    initBoard(gameBoard)
    sideToPlay = 0
    fen1 = Tk()
    fen1.title("test")
    can1 = Canvas(fen1, bg='white', height=300, width=300)
    can1.pack(side=LEFT)
    printBoardUI(gameBoard)
    fen1.mainloop()

mainWithUI()
