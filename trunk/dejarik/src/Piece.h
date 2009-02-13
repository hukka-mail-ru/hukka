#ifndef PIECE_H_
#define PIECE_H_

#include "Cell.h"
#include "Player.h"
#include "Macros.h"


CLASSPTR(Cell)
CLASSPTR(Player)
CLASSPTR(Piece)


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
    
    unsigned getAttackRating() const;
    unsigned getDefenceRating() const;
    unsigned getMoveRating() const;
    const CellPtr& getPosition() const;
    const PlayerPtr& getPlayer() const;
    const std::string getName() const;
    
    void push();
    void setPosition(const CellPtr& cell);
    void setPlayer(const PlayerPtr& player);
    void setMoveRating(unsigned move);

private:
    
    std::string mName;
    PlayerPtr mPlayer;
    CellPtr mPosition;    
    
    unsigned mAttackRating;
    unsigned mDefenceRating;
    unsigned mMoveRating;
    
};


#endif /*PIECE_H_*/
