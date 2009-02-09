#ifndef CELL_H_
#define CELL_H_

#include <boost/shared_ptr.hpp> 
#include "Piece.h"

struct Cell;
class Piece;

typedef boost::shared_ptr<Cell> CellPtr;
typedef boost::shared_ptr<Piece> PiecePtr;

struct Cell
{
    Cell(unsigned circle, unsigned xnum):
        c(circle),
        x(xnum),
        mark(false) // not marked initially
    {
        prev.reset();
        piece.reset();
    }
    
    // operator = needed 
    
    unsigned c; // coordinate: 0,1,2
    unsigned x; // coordinate: 0 in circle 0;   0..11 in circle 1;  0..11 in circle 2.  
    
    bool mark;
    CellPtr prev;
    
    PiecePtr piece;
};



#endif /*CELL_H_*/
