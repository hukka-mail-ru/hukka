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
    SEL_POSSIBLE_PUSH
};

struct Cell
{
public:
    Cell(unsigned circle, unsigned radius);
    
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
    
private:
    
    void createSegment(float radius1, float radius2);
};



#endif /*CELL_H_*/
