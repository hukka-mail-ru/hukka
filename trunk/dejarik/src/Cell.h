#ifndef CELL_H_
#define CELL_H_

#include <boost/shared_ptr.hpp> 
#include "Piece.h"


struct Cell
{
    Cell(unsigned circle, unsigned xnum):
        c(circle),
        x(xnum),
        mark(false) // not marked initially
    {}
    
    unsigned c; // coordinate: 0,1,2
    unsigned x; // coordinate: 0 in circle 0;   0..11 in circle 1;  0..11 in circle 2.  
    
    bool mark;
    
    PiecePtr piece;
};

typedef boost::shared_ptr<Cell> CellPtr;

#endif /*CELL_H_*/
