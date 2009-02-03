#ifndef BOARD_H_
#define BOARD_H_

#include <vector>
#include "Cell.h"

#define CIRCLE 12 // cells in a circle

class Board // only one board in game
{
    friend class DejaricTest;
    
public:
    
    
    Board()
    {
        CellPtr cell0 (new Cell(0,0));
        cells.push_back(cell0);
        
        for(unsigned i=0; i<CIRCLE; i++)
        {
            CellPtr cell1 (new Cell(1,i));
            cells.push_back(cell1);
            
            CellPtr cell2 (new Cell(2,i));
            cells.push_back(cell2);
        }       
    }
    
    
private:
    std::vector<CellPtr> cells;
};

#endif /*BOARD_H_*/
