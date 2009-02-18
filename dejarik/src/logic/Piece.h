#ifndef PIECE_H_
#define PIECE_H_

#include "Cell.h"
#include "Player.h"
#include "../include/Macros.h"


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
          moveRating(move)
          {
              cell.reset();
          }    
    
    std::string name;
    PlayerPtr player;
    CellPtr cell;    
    
    unsigned attackRating;
    unsigned defenceRating;
    unsigned moveRating;
    
};


#endif /*PIECE_H_*/
