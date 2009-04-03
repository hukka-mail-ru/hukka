#ifndef CELL_H_
#define CELL_H_

#include "Macros.h"
#include "Piece.h"

#include <vector>

#define PI 3.14159265

// in pixels
#define CIRCLE_CENTER_X 121.0
#define CIRCLE_CENTER_Y 158.0

#define RADIUS_1 27.0 
#define RADIUS_2 76.0
#define RADIUS_3 114.0

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


class Cell
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
