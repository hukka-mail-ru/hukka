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
    friend class TestPlayer;
public:
    Player(const std::string& name, const BoardPtr& board): 
        mName(name),
        mBoard(board) 
        {}
    
    void addPiece(const PiecePtr& piece)
    {
        mPieces.push_back(piece);
    }
    
    unsigned howManyPieces()
    {
        return mPieces.size();
    }
    
    /* 
     Define the cell: empty ? ally ? enemy ?
      
    if clicked on ally, we must do TURN_START, even we are obtained TURN_FINISH
    
    on TURN_START:
       select mActivePiece
       player gets possible moves 
    
    on TURN_FINISH:
      player moves mActivePiece 
        step-by-step, in cycle, mActivePiece.setPosition
      or attacks an enemy piece:
        enemy = get enemy piece 
        computeBattleRusult
        res. can be: enemy.push or enemy.kill
        res. can be: mActivePiece.push or mActivePiece.kill
    */
    bool makeTurn(unsigned c, unsigned x, TurnStage turnStage);
    
private:
    
    bool moveActivePiece(unsigned c, unsigned x);
    
    bool attackEnimy(const PiecePtr& myPiece, const PiecePtr& enemyPiece) { return true; }
    
    void computeBattleResult() {}
    
    std::string mName;
    
    BoardPtr mBoard;
    
    std::vector<PiecePtr> mPieces;
    
    PiecePtr mActivePiece;
};

CLASSPTR(Player);




#endif /*PLAYER_H_*/
