#include <math.h>
#include "Cell.h"



#define INTERIM_ANGLES 4 // smoothness of the circles

using namespace std;



Cell::Cell(unsigned circle, unsigned radius):
    c(circle),
    r(radius),
    mark(0), // not marked initially
    selected(SEL_NONE) // not selected initially
{
    prev.reset();
    piece.reset();

    
    // CENTRAL CIRCLE
    if(c == 0)
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
    else if(c == 1)
    {
        createSegment(RADIUS_1, RADIUS_2);
    }
    // CIRCLE 2
    else if(c == 2)
    {        
        createSegment(RADIUS_2, RADIUS_3);
    }
}


void Cell::createSegment(float radius1, float radius2)
{
    // angles
    const float a1 = PI/6.0 * r;
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
