#include "Animation.h"

/*
void Animation::initPiece(const PieceImagePtr& piece)
{
    piece->angle = getNormalAngle(cellImage->x_center, cellImage->y_center);
    piece->x = cellImage->x_center;
    piece->y = cellImage->y_center;
}

void Animation::updatePiece(const PieceImagePtr& pieceImage)
{
    TRY_BEGINS;
    
    
    const unsigned rot = 20;
    const unsigned straight = 20;
    const unsigned total = rot + straight;
   
    static unsigned moves = 0; // pixel by pixel
    static unsigned step = 0; // cell by cell

    if(pieceImagepiece->cellBeforeMoving != cell) // moving needed
    {
        assert(mMoveSteps.size() > 1);    
        
        
        //  without change of direction
        if(moves < rot && (int)cell->piece->angle == 
            (int)(getNormalAngle(cell->piece->x, cell->piece->y) + getRotation(step)) )
        {
            moves = rot;
        }
        
        // rotation at the beginning
        if(moves < rot)
        {            
            float a_start = cell->piece->angle;            
            float a_finish = getNormalAngle(cell->piece->x, cell->piece->y) + getRotation(step);
            
            if(mMoveSteps[step]->c == 0) // special case - when we start from the center
            {
                a_finish = getNormalAngle(mMoveSteps[step+1]->x_center, mMoveSteps[step+1]->y_center); 
                           + getRotation(step);
                   
                a_finish = shorterAngle(a_finish);
                
                cout << "a_start: " << a_start << endl;
                cout << "a_finish: " << a_finish << endl;
            }          
            
            const float a = a_start + (a_finish - a_start) / rot * moves;
            
            cell->piece->angle = a;
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
            cell->piece->angle = getNormalAngle(x, y) + getRotation(step);
            cell->piece->x = x;
            cell->piece->y = y;
        }
        
        
        // draw
        Video::drawSprite(piece->name, color, XY_CENTER, piece->x,  piece->y, piece->angle); 

        moves++;
        
        if(moves >= total) // proceed to the next cell
        {            
            step++;
            moves = 0;
            
            if(step == mMoveSteps.size() - 1) // finish cell reached
            {
                step = 0;
                mMoving = false;
                cell->piece->cellBeforeMoving = cell;
            }
        }
    }    
    else // just draw a piece
    {
          
    }
    TRY_RETHROW;
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


float Animation::shorterAngle(float ang)
{

    if(ang > 180)
        return 180 - ang;
    
    if(ang < -180)
        return 360 + ang;

    return ang;
}*/

void Animation::updateAll()
{
    
}
