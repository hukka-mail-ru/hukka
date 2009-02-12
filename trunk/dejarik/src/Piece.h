#ifndef PIECE_H_
#define PIECE_H_

#include "Cell.h"
#include "Player.h"
#include "Macros.h"

CLASSPTR(Cell);
CLASSPTR(Player);

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
    const Player* getPlayer() const;
    const std::string getName() const;
    
    void push();
    void setPosition(const CellPtr& cell);
    void setPlayer(Player* player);

private:
    
    std::string mName;
    Player* mPlayer;
    CellPtr mPosition;    
    
    unsigned mAttackRating;
    unsigned mDefenceRating;
    unsigned mMoveRating;
    
};

CLASSPTR(Piece);
//typedef boost::shared_ptr<Piece> PiecePtr;

#endif /*PIECE_H_*/
