#include <math.h>
#include "CellImage.h"



#define INTERIM_ANGLES 4 // smoothness of the circles

using namespace std;



CellImage::CellImage(const CellPtr& _cell)
{
    cell = _cell;
    
    // CENTRAL CIRCLE
    if(cell->c == 0)
    {
        for(unsigned i = 0; i < INTERIM_ANGLES*3; i++)
        {
            float a = PI/6.0*i;
            x.push_back(CIRCLE_CENTER_X + RADIUS_1 * cos(a));
            y.push_back(CIRCLE_CENTER_Y + RADIUS_1 * sin(a));
        }
        
        x_center = CIRCLE_CENTER_X;
        y_center = CIRCLE_CENTER_Y;
    }
    // CIRCLE 1
    else if(cell->c == 1)
    {
        createSegment(RADIUS_1, RADIUS_2);
    }
    // CIRCLE 2
    else if(cell->c == 2)
    {        
        createSegment(RADIUS_2, RADIUS_3);
    }
}


void CellImage::createSegment(float radius1, float radius2)
{
    // angles
    const float a1 = PI/6.0 * cell->r;
    const float a2 = a1 + PI/6.0;
    const float interim = (a2 - a1)/INTERIM_ANGLES;        
    
    float a = a1;
    for(unsigned i = 0; i<=INTERIM_ANGLES; i++) // BBBBBB
    {
        x.push_back(CIRCLE_CENTER_X + radius2 * cos(a));
        y.push_back(CIRCLE_CENTER_Y + radius2 * sin(a));
        a += interim;
    }

    for(unsigned i = 0; i<=INTERIM_ANGLES; i++) // DDDDDD
    {            
        a -= interim;
        x.push_back(CIRCLE_CENTER_X + radius1 * cos(a));
        y.push_back(CIRCLE_CENTER_Y + radius1 * sin(a));
    }
    
    // define center
    x_center = CIRCLE_CENTER_X + (radius2 + radius1)/2 * cos((a2 + a1)/2);
    y_center = CIRCLE_CENTER_Y + (radius2 + radius1)/2 * sin((a2 + a1)/2);
    
    
}
