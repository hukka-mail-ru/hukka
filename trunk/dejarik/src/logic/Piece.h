#ifndef PIECE_H_
#define PIECE_H_

#include "Cell.h"
#include "Player.h"
#include "../common/Macros.h"


CLASSPTR(Cell)
CLASSPTR(Player)
CLASSPTR(Piece)


struct Piece
{
public:
    
    Piece(const std::string& name,
          unsigned attack, unsigned defence, unsigned move):
          name(name),
          attackRating(attack),
          defenceRating(defence),
          moveRating(move),
          angle(FLOAT_UNDEFINED),
          x(FLOAT_UNDEFINED),
          y(FLOAT_UNDEFINED)
          {
              cell.reset();
          }    
    
    std::string name;
    PlayerPtr player;
    CellPtr cell;   
    CellPtr cellBeforeMoving;     
    
    unsigned attackRating;
    unsigned defenceRating;
    unsigned moveRating;
    
    float angle; // used by UI
    float x; // used by UI
    float y; // used by UI
    
};


#endif /*PIECE_H_*/
