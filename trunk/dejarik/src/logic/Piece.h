 #ifndef PIECE_H_
#define PIECE_H_

#include "Cell.h"
#include "Player.h"
#include "../common/Macros.h"


CLASSPTR(Cell)
CLASSPTR(Player)
CLASSPTR(Piece)


class Piece
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
          y(FLOAT_UNDEFINED),
          sprite(0),
          sprites(8)
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
    unsigned sprite;
    unsigned sprites;
    
};


#endif /*PIECE_H_*/
