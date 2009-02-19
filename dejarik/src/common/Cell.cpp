#include <math.h>
#include "Cell.h"

#define PI 3.14159265

#define CELL_RADIUS_1 0.5 
#define CELL_RADIUS_2 1.5
#define CELL_RADIUS_3 2.5

#define INTERIM_ANGLES 3

using namespace std;

Cell::Cell(unsigned circle, unsigned radius):
    c(circle),
    r(radius),
    mark(0) // not marked initially
{
    prev.reset();
    piece.reset();

    
    // CENTRAL CIRCLE
    if(c == 0)
    {
        for(unsigned i = 0; i < INTERIM_ANGLES*4; i++)
        {
            float a = PI/6.0*i;
            x.push_back(CELL_RADIUS_1 * cos(a));
            y.push_back(CELL_RADIUS_1 * sin(a));
        }
    }
    // CIRCLE 1
    else if(c == 1)
    {
        // angles
        const float a1 = PI/6.0 * r;
        const float a2 = a1 + PI/6.0;
                
        float a = a1;
        for(unsigned i = 0; i<INTERIM_ANGLES; i++) // BBBBBB
        {
            a += (a2 - a1)/INTERIM_ANGLES*i;
            x.push_back(CELL_RADIUS_2 * cos(a));
            y.push_back(CELL_RADIUS_2 * sin(a));
        }

        for(unsigned i = 0; i<INTERIM_ANGLES; i++) // DDDDDD
        {
            a -= (a2 - a1)/INTERIM_ANGLES*i;
            x.push_back(CELL_RADIUS_1 * cos(a));
            y.push_back(CELL_RADIUS_1 * sin(a));
        }
    }
    // CIRCLE 2
    else if(c == 2)
    {        
        const float a1 = PI/6.0 * r;
        const float a2 = a1 + PI/6.0;
                
        float a = a1;
        for(unsigned i = 0; i<INTERIM_ANGLES; i++) // BBBBBB
        {
            a += (a2 - a1)/INTERIM_ANGLES*i;
            x.push_back(CELL_RADIUS_3 * cos(a));
            y.push_back(CELL_RADIUS_3 * sin(a));
        }

        for(unsigned i = 0; i<INTERIM_ANGLES; i++) // DDDDDD
        {
            a -= (a2 - a1)/INTERIM_ANGLES*i;
            x.push_back(CELL_RADIUS_2 * cos(a));
            y.push_back(CELL_RADIUS_2 * sin(a));
        }
    }

}
