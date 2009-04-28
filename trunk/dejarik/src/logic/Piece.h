 #ifndef PIECE_H_
#define PIECE_H_

#include "Cell.h"
#include "Player.h"
#include "../system/Macros.h"


CLASSPTR(Cell)
CLASSPTR(Player)
CLASSPTR(Piece)

enum PieceName
{
    PN_MONNOK = 0,
    PN_MOLATOR = 1,
    PN_KLORSLUG = 2,
    PN_HOUJIX = 3,
    PN_GHHK = 4,
    PN_STRIDER = 5,
    PN_SARVIP = 6,
    PN_NGOK = 7
};

struct Piece
{
    Piece(PieceName name,
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
    
    PieceName name;
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
