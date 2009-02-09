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
    
    unsigned getAttackRating();
    unsigned getDefenceRating();
    unsigned getMoveRating();
    CellPtr& getPosition();
    PlayerPtr& getPlayer();
    
    void setPosition(const CellPtr& cell);
    void setPlayer(const PlayerPtr& player);

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
