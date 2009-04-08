#include "Animation.h"
#include <math.h>

using namespace std;

bool Animation::updateAll(const std::vector<CellPtr>& moveSteps)
{
    TRY_BEGINS;
    
    mMoveSteps = moveSteps;
    
    // draw Pieces
    vector<PiecePtr> pieces = mGame->getBoard()->getPieces();
    for(unsigned i = 0; i < pieces.size(); i++)
    {
        initPiece(pieces[i]);
        updatePiece(pieces[i]);
    }
    
    return mMoving;
    
    TRY_RETHROW;
}


void Animation::initPiece(const PiecePtr& piece)
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


float Animation::getTargetAngle(unsigned step)
{
    if(mMoveSteps[step]->c == 0)
    {
        float res = getNormalAngle(mMoveSteps[step+1]->x_center, mMoveSteps[step+1]->y_center) + getRotation(step);
        return shorterAngle(res);
    }
    else
    {
        float res = getNormalAngle(mMoveSteps[step]->x_center, mMoveSteps[step]->y_center) + getRotation(step);
        return shorterAngle(res);
    }

    
}

float Animation::getSmallestAngle(float start, float end)
{
    start = shorterAngle(start);
    end = shorterAngle(end);
    
    cout << " start : " << start << endl;
    cout << " end : " << end << endl;
    if(end == 0)
        end = 360;
    
    float res = 0;
    float shifted = end - start;
    if(shifted > 180)
        res = -(360 - shifted);
    else
        res = shifted;
    
    cout << " getSmallestAngle : " << res << endl;
    
    assert(fabs(res) <= 360);
    
    // shorter angle
    if(fabs(res) > 180)
        res = (fabs(res) - 180) * (-res / fabs(res));
    
    cout << " res : " << res << endl;
    
    assert(fabs(res) <= 180);
    
    return res;
}

void Animation::updatePieceImage(const PiecePtr& piece)
{
    TRY_BEGINS;
    /*
    if(piece->name != "Molator") // TODO temorary
        return;
    
    piece->sprite++;
    
    if(piece->sprite == piece->sprites)
        piece->sprite = 0; 
    */
    TRY_RETHROW;
}

void Animation::updatePiece(const PiecePtr& piece)
{
    TRY_BEGINS;

    if(piece->cellBeforeMoving == piece->cell) // moving not needed
    {
        return;
    }

    const float oldx = piece->x;
    const float oldy = piece->y;
    const float oldang = piece->angle;
    
    const unsigned rot = 10;
    const unsigned straight = 20;
    const unsigned total = rot + straight;
   
    static unsigned moves = 1; // pixel by pixel
    static unsigned step = 0; // cell by cell
    
    assert(mMoveSteps.size() > 1);    
    mMoving = true;
    
    // 1. Rotation
    if(moves <= rot)
    {
       float targetAngle = getTargetAngle(step);
    
       if((int)shorterAngle(piece->angle) != (int)targetAngle) // need to rotate the piece 
       {
           piece->angle = piece->angle + getSmallestAngle(piece->angle, targetAngle) / rot * moves;           
           moves++;
       }
       else // piece has the proper position
       {
           moves = rot + 1;
       }
    }
    
   // 2. Straight
   if(moves > rot && moves <= total)
   {
       const float x_start = mMoveSteps[step]->x_center; 
       const float y_start = mMoveSteps[step]->y_center; 
       
       const float x_finish = mMoveSteps[step+1]->x_center; 
       const float y_finish = mMoveSteps[step+1]->y_center; 
       
       piece->x = x_start + (x_finish - x_start) / straight * (moves - rot);
       piece->y = y_start + (y_finish - y_start) / straight * (moves - rot);
       
       if(mMoveSteps[step+1]->c != 0) // don't need to change the angle in the center
           piece->angle = getNormalAngle(piece->x, piece->y) + getRotation(step);
       
       moves++;
   }

   updatePieceImage(piece);

    assert(oldx != piece->x || oldy != piece->y || oldang != piece->angle);
  //  assert(fabs(oldang - piece->angle) < 15); // TODO sometimes we crash here
    assert(piece->x < CIRCLE_CENTER_X + RADIUS_3);
    assert(piece->x > CIRCLE_CENTER_X - RADIUS_3); 
    assert(piece->y < CIRCLE_CENTER_Y + RADIUS_3);
    assert(piece->y > CIRCLE_CENTER_Y - RADIUS_3); 
    
    if(moves > total) // proceed to the next cell
    {            
        step++;
        moves = 1;
        
        if(step == mMoveSteps.size() - 1) // finish cell reached
        {
            step = 0;
            mMoving = false;
            piece->cellBeforeMoving = piece->cell;
        }
    }

    TRY_RETHROW;
}

// helper for drawPiece
float Animation::getNormalAngle(float x, float y)
{   
    if(x == 0 && y ==0)
        return 0.0;
    
    float ang = atan ( fabs(y)/fabs(x) ) * 180.0 / PI;

    if(x >= 0 && y >= 0)
    {
        ang = ang + 90;
    }
    else if (x < 0 && y >= 0)
    {
    	ang = -ang - 90;
    }
    else if (x < 0 && y < 0)
    {
    	ang = ang - 90;
    }
    else if (x >= 0 && y < 0)
    {
    	ang = -ang + 90;
    }
    
    return ang;
}

// helper for drawPiece
float Animation::getRotation(unsigned step)
{
    TRY_BEGINS;
    
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
    
    TRY_RETHROW
}

float Animation::shorterAngle(float ang)
{
    if(ang > 0)
    {
        while(ang >= 360)
            ang -= 360;
    }
    else
    {
        while(ang < 0)
            ang += 360;
    }

    return ang;
}
