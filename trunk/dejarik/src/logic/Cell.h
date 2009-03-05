#ifndef CELL_H_
#define CELL_H_

#include "Macros.h"
#include "Piece.h"


CLASSPTR(Cell);
CLASSPTR(Piece);

enum Selection
{
    SEL_NONE,
    SEL_CLICKED,
    SEL_POSSIBLE_MOVE,
    SEL_POSSIBLE_TARGET,
    SEL_POSSIBLE_PUSH
};


struct Cell
{
    Cell(unsigned circle, unsigned radius):
        c(circle),
        r(radius),
        mark(0), // not marked initially
        selected(SEL_NONE) // not selected initially
        {
            prev.reset();
            piece.reset();
        }

    unsigned c; // coordinate: 0,1,2
    unsigned r; // coordinate: 0 in circle 0;   0..11 in circle 1;  0..11 in circle 2.  
        
    unsigned mark;
    CellPtr prev;
    
    PiecePtr piece;
    
    Selection selected; // by mouse or 
};



#endif /*CELL_H_*/
