#ifndef CELLIMAGE_H_
#define CELLIMAGE_H_

#include "../common/Macros.h"
#include "Cell.h"

#define PI 3.14159265

// in pixels
#define CIRCLE_CENTER_X 121.0
#define CIRCLE_CENTER_Y 158.0

#define RADIUS_1 27.0 
#define RADIUS_2 76.0
#define RADIUS_3 114.0

class CellImage
{
public:
    CellImage(const CellPtr& _cell);
    
    // polygon vertexes
    std::vector<float> x;
    std::vector<float> y;
    
    // polygon center
    float x_center;
    float y_center;
    
    CellPtr cell;
    
private:
    
    void createSegment(float radius1, float radius2);
};

CLASSPTR(CellImage);

#endif /*CELLIMAGE_H_*/
