#ifndef PIECEIMAGE_H_
#define PIECEIMAGE_H_

#include "../common/Macros.h"
#include "Piece.h"

CLASSPTR(CellImage);

class PieceImage
{
public:
    
    PieceImage(const PiecePtr& _piece): 
        piece(_piece),
        angle(FLOAT_UNDEFINED),
        x(FLOAT_UNDEFINED),
        y(FLOAT_UNDEFINED) {}
    
    PiecePtr piece;
    
    float angle;
    float x; 
    float y;
};

CLASSPTR(PieceImage);

#endif /*PIECEIMAGE_H_*/
