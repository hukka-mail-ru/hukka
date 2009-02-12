#ifndef PLAYER_H_
#define PLAYER_H_

#include <string>
#include <vector>
#include "Macros.h"
#include "Piece.h"
#include "Board.h"

// e.g. a move for 1.0 to 2.0
// =>  TS_START at 1.0, TS_FINISH at 2.0
enum TurnStage 
{
    TURN_START, 
    TURN_FINISH
};

CLASSPTR(Piece);
CLASSPTR(Board);

class Player
{
public:
    Player(const std::string& name, const BoardPtr& board): 
        mName(name),
        mBoard(board) 
        {}
    
    void addPiece(const PiecePtr& piece)
    {
        mPieces.push_back(piece);
    }
    
    /* 
     Define the cell: empty ? ally ? enimy ?
      
    if clicked on ally, we must do TURN_START, even we are obtained TURN_FINISH
    
    on TURN_START:
       select mActivePiece
       player gets possible moves 
    
    on TURN_FINISH:
      player moves mActivePiece 
        step-by-step, in cycle, mActivePiece.setPosition
      or attacks an enimy piece:
        enimy = get enimy piece 
        computeBattleRusult
        res. can be: enimy.push or enimy.kill
        res. can be: mActivePiece.push or mActivePiece.kill
    */
    bool makeTurn(unsigned c, unsigned x, TurnStage turnStage);
    
private:
    
    void moveActivePiece();
    
    void attackEnimyPiece();
    
    void computeBattleResult();
    
    std::string mName;
    
    BoardPtr mBoard;
    
    std::vector<PiecePtr> mPieces;
    
    PiecePtr mActivePiece;
};

CLASSPTR(Player);




#endif /*PLAYER_H_*/
