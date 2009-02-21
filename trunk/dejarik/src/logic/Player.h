#ifndef PLAYER_H_
#define PLAYER_H_

#include <string>
#include <vector>
#include "../common/Macros.h"
#include "Piece.h"
#include "Board.h"
#include "Cell.h"


CLASSPTR(Piece);
CLASSPTR(Board);
CLASSPTR(Player);
CLASSPTR(Cell);

#ifdef UNIT_TESTS
extern bool TestMakePush;
extern bool TestMakeCounterPush;
#endif

// e.g. a move for 1.0 to 2.0
// =>  TURN_SELECTION at 1.0, TURN_ACTION at 2.0
enum TurnStage 
{
    TURN_SELECTION, 
    TURN_ACTION
};

enum BattleResult
{
    RES_NO_BATTLE,
    RES_KILL,
    RES_PUSH,
    RES_COUNTER_KILL,
    RES_COUNTER_PUSH
};

class Player
{
    friend class TestPlayer;
    friend class TestGame;
    
public:
    Player(const std::string& name, const BoardPtr& board): 
        mName(name),
        mBoard(board) 
        {}
    
    void addPiece(const PiecePtr& piece);
    
    void removePiece(const PiecePtr& piece);
    
    unsigned howManyPieces();
    
    const PiecePtr& getActivePiece()
    {
        return mActivePiece;
    }
    
    void setActivePiece(const PiecePtr& piece)
    {
        mActivePiece = piece;
    }

    
    void setLeftMoves(unsigned moves)
    {
        mLeftMoves = moves;
    }
    
    void incrementLeftMoves()
    {
        mLeftMoves++;
    }
    
    void decrementLeftMoves()
    {
        mLeftMoves--;
    }

    unsigned getLeftMoves()
    {
        return mLeftMoves;
    }

    void movePiece(const CellPtr& cell);    
    
    BattleResult attackEnimy(const PiecePtr& enemyPiece);
    
private:
    

    
    /*
        if Attack beats Defense by 7 or more then Kill
        if Attack beats Defense by 6 or less then Push
        if a draw then Counter-Push
        if Defense beats Attack by 6 or less then Counter-Push
        if Defense beats attack by 7 or less then Counter-Kill
    */
    BattleResult getBattleResult(unsigned attack, unsigned defence);
       
    std::string mName;
    
    BoardPtr mBoard;
    
    std::vector<PiecePtr> mPieces;
    
    PiecePtr mActivePiece;
    
    unsigned mLeftMoves;
};



#endif /*PLAYER_H_*/
