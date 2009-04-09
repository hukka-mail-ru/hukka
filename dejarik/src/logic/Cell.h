#ifndef CELL_H_
#define CELL_H_

#include "Macros.h"
#include "Piece.h"

#include <vector>


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
    
    // operator = needed 
    
    unsigned c; // coordinate: 0,1,2
    unsigned r; // coordinate: 0 in circle 0;   0..11 in circle 1;  0..11 in circle 2.  
    
    // polygon vertexes
    std::vector<float> x;
    std::vector<float> y;
    
    // polygon center
    float x_center;
    float y_center;
    
    unsigned mark;
    CellPtr prev;
    
    PiecePtr piece;
    
    Selection selected; // by mouse or 

};



#endif /*CELL_H_*/
