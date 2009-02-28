#include <math.h>
#include "Cell.h"

#define PI 3.14159265

// in pixels
#define SCREEN_WIDTH  240
#define SCREEN_HEIGHT 320

#define SCREEN_CENTER_X SCREEN_WIDTH/2 + 1
#define SCREEN_CENTER_Y SCREEN_HEIGHT/2 - 2

#define CELL_RADIUS_1 27 
#define CELL_RADIUS_2 76
#define CELL_RADIUS_3 114

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
            x.push_back(SCREEN_CENTER_X + CELL_RADIUS_1 * cos(a));
            y.push_back(SCREEN_CENTER_Y + CELL_RADIUS_1 * sin(a));
        }
        
        x_center = SCREEN_CENTER_X;
        y_center = SCREEN_CENTER_Y;
    }
    // CIRCLE 1
    else if(c == 1)
    {
        createSegment(CELL_RADIUS_1, CELL_RADIUS_2);
    }
    // CIRCLE 2
    else if(c == 2)
    {        
        createSegment(CELL_RADIUS_2, CELL_RADIUS_3);
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
        x.push_back(SCREEN_CENTER_X + radius2 * cos(a));
        y.push_back(SCREEN_CENTER_Y + radius2 * sin(a));
        a += interim;
    }

    for(unsigned i = 0; i<=INTERIM_ANGLES; i++) // DDDDDD
    {            
        a -= interim;
        x.push_back(SCREEN_CENTER_X + radius1 * cos(a));
        y.push_back(SCREEN_CENTER_Y + radius1 * sin(a));
    }
    
    // define center
    x_center = SCREEN_CENTER_X + (radius2 + radius1)/2 * cos((a2 + a1)/2);
    y_center = SCREEN_CENTER_Y + (radius2 + radius1)/2 * sin((a2 + a1)/2);
    
    
}
