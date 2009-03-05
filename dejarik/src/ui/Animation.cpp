#include "Animation.h"
#include <math.h>

using namespace std;

bool Animation::updateAll(const std::vector<CellPtr>& moveSteps)
{
    mMoveSteps = moveSteps;
    
    // draw Pieces
    vector<PiecePtr> pieces = mGame->getPieces();
    for(unsigned i = 0; i < pieces.size(); i++)
    {
        updatePiece(pieces[i]);
    }
    
    return mMoving;
}

void Animation::updatePiece(const PiecePtr& piece)
{
    TRY_BEGINS;
    
    const unsigned rot = 20;
    const unsigned straight = 20;
    const unsigned total = rot + straight;
   
    static unsigned moves = 0; // pixel by pixel
    static unsigned step = 0; // cell by cell

    if(piece->cellBeforeMoving != piece->cell) // moving needed
    {
        assert(mMoveSteps.size() > 1);    
        mMoving = true;
        
        //  without change of direction
        if(moves < rot && (int)piece->angle == 
            (int)(getNormalAngle(piece->x, piece->y) + getRotation(step)) )
        {
            moves = rot;
        }
        
        // rotation at the beginning
        if(moves < rot)
        {            
            float a_start = piece->angle;            
            float a_finish = getNormalAngle(piece->x, piece->y) + getRotation(step);
            
            if(mMoveSteps[step]->c == 0) // special case - when we start from the center
            {
                a_finish = getNormalAngle(mMoveSteps[step+1]->x_center, mMoveSteps[step+1]->y_center); 
                           + getRotation(step);
                   
                a_finish = shorterAngle(a_finish);
                
                cout << "a_start: " << a_start << endl;
                cout << "a_finish: " << a_finish << endl;
            }          
            
            const float a = a_start + (a_finish - a_start) / rot * moves;
            
            piece->angle = a;
        }
        
        // then move straight
        if(moves >= rot && moves < total)
        {
            const float x_start = mMoveSteps[step]->x_center; 
            const float y_start = mMoveSteps[step]->y_center; 
            
            const float x_finish = mMoveSteps[step+1]->x_center; 
            const float y_finish = mMoveSteps[step+1]->y_center; 
            
            float x = x_start + (x_finish - x_start) / straight * (moves - rot);
            float y = y_start + (y_finish - y_start) / straight * (moves - rot);
            
            // smooth rotation     
            piece->angle = getNormalAngle(x, y) + getRotation(step);
            piece->x = x;
            piece->y = y;
        }
                
        moves++;
        
        
        if(moves >= total) // proceed to the next cell
        {            
            step++;
            moves = 0;
            
            if(step == mMoveSteps.size() - 1) // finish cell reached
            {
                step = 0;
                mMoving = false;
                piece->cellBeforeMoving = piece->cell;
            }
        }
    }    
    else // just draw a piece
    {
        if(piece->angle == FLOAT_UNDEFINED) 
        {
            piece->angle = getNormalAngle(piece->cell->x_center, piece->cell->y_center);
        }
        if(piece->x == FLOAT_UNDEFINED) 
        {
            piece->x = piece->cell->x_center;
        }
        if(piece->y == FLOAT_UNDEFINED) 
        {
            piece->y = piece->cell->y_center;
        }
    }
    TRY_RETHROW;
}

// helper for drawPiece
float Animation::getNormalAngle(float x, float y)
{
    const float dy = y - CIRCLE_CENTER_Y;
    const float dx = x - CIRCLE_CENTER_X;
    
    
    
    float ang = atan (- dy / dx ) * 180.0 / PI;
    if(ang > 0)
        ang += 180;
    
    if(dy < 0)
        ang += 180;
    
    ang +=90;
    
    return ang;
}

// helper for drawPiece
float Animation::getRotation(unsigned step)
{
    float rotation = 0.0;
    if(mMoveSteps[step]->c > mMoveSteps[step+1]->c) // look to the center
    {
        rotation = 0;
    }
    else if(mMoveSteps[step]->c < mMoveSteps[step+1]->c) // look to the outer space
    {
        rotation = 180.0;
    }
    else if(mMoveSteps[step]->r == RADIUSES-1 && mMoveSteps[step+1]->r == 0) // boundary
    {
        rotation = 90.0;
    }
    else if(mMoveSteps[step]->r == 0 && mMoveSteps[step+1]->r == RADIUSES-1) // boundary
    {
        rotation = -90.0;
    }
    else if(mMoveSteps[step]->r < mMoveSteps[step+1]->r)// look left
    {
        rotation = 90.0;
    }
    else if(mMoveSteps[step]->r > mMoveSteps[step+1]->r)// look right
    {
        rotation = - 90.0;
    }
    
    return rotation;
}

float Animation::shorterAngle(float ang)
{

    if(ang > 180)
        return 180 - ang;
    
    if(ang < -180)
        return 360 + ang;

    return ang;
}
