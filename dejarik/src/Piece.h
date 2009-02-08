#ifndef PIECE_H_
#define PIECE_H_

#include <boost/shared_ptr.hpp> 
#include "Cell.h"
#include "Player.h"

class Cell;
typedef boost::shared_ptr<Cell> CellPtr;

class Piece
{
public:
    
    Piece(const std::string& name,
          unsigned attack, unsigned defence, unsigned move):
          mName(name),
          mAttackRating(attack),
          mDefenceRating(defence),
          mMoveRating(move)
          {
              mPosition.reset();
          }
    
    unsigned getAttackRating()
    {
        return mAttackRating;
    }
    
    unsigned getDefenceRating()
    {
        return mDefenceRating;
    }
    
    unsigned getMoveRating()
    {
        return mMoveRating;
    }

    CellPtr& getPosition()
    {
        return mPosition;
    }
    
    void setPosition(const CellPtr& cell)
    {
        mPosition = cell;
    }

    PlayerPtr& getPlayer()
    {
        return mPlayer;
    }
    
    void setPlayer(const PlayerPtr& player)
    {
        mPlayer = player;
    }


private:
    
    std::string mName;
    PlayerPtr mPlayer;
    CellPtr mPosition;    
    
    unsigned mAttackRating;
    unsigned mDefenceRating;
    unsigned mMoveRating;
    
};

typedef boost::shared_ptr<Piece> PiecePtr;

#endif /*PIECE_H_*/
