#ifndef ANIMATION_H_
#define ANIMATION_H_

#include "PieceImage.h"
#include "Cell.h"

class Animation
{
public:
    
    void updateAll();
    
private:
  
    float getNormalAngle(float x, float y); // must be changed smoothly (when moving along orbit)
    float getRotation(unsigned step);
    float shorterAngle(float ang);
    
    void initPiece(const PieceImagePtr& piece);
    void updatePiece(const CellPtr& cell);

};


#endif /*ANIMATION_H_*/
