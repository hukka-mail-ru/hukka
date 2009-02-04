#ifndef PIECE_H_
#define PIECE_H_

#include <boost/shared_ptr.hpp> 
#include "Cell.h"

class Cell;
typedef boost::shared_ptr<Cell> CellPtr;

class Piece
{
public:
    
    Piece() {}
    
    void setPosition(const CellPtr& pos)
    {
        position = pos;
    }
    
    CellPtr& getPosition()
    {
        return position;
    }
    
private:
    
    CellPtr position;
    
};

typedef boost::shared_ptr<Piece> PiecePtr;

#endif /*PIECE_H_*/
