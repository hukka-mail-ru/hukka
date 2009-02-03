#ifndef CELL_H_
#define CELL_H_

#include <boost/shared_ptr.hpp> 

class Cell
{
public:
    Cell(unsigned circle, unsigned num):
        mCircle(circle),
        mNum(num)
    {}

private:
    
    unsigned mCircle; // coordinate: 0,1,2
    unsigned mNum;    // coordinate: 0 in circle 0;   0..11 in circle 1;  0..11 in circle 2.  
    
    //Piece* piece;
};

typedef boost::shared_ptr<Cell> CellPtr;

#endif /*CELL_H_*/
