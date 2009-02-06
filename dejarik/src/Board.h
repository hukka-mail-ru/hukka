#ifndef BOARD_H_
#define BOARD_H_

#include <vector>
#include "Cell.h"

#define CIRCLE 12 // cells in a circle

class Board // only one board in game
{
    friend class DejaricTest;
    
public:
    
    
    Board();
    
    CellPtr& getCell(unsigned c, unsigned x);    
    
    bool isMoveValid(const PiecePtr& piece, const CellPtr& dst);
    
    void getPossibleMoves(const CellPtr& pos, unsigned max, std::vector<CellPtr>& moves);
    
    void getMoveSteps(const CellPtr& start, const CellPtr& finish, std::vector<CellPtr>& steps);
    
private:
    std::vector<CellPtr> cells;
};

#endif /*BOARD_H_*/
