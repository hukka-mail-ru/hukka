#include <sstream>
#include "Board.h"

using namespace std;

Board::Board()
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

CellPtr& Board::getCell(unsigned c, unsigned x)
{
    for(unsigned i=0; i<cells.size(); ++i)
    {
        if(cells[i]->c == c && cells[i]->x == x)
        {
            return cells[i];
        }
    }
    
    stringstream err;
    err << "No cell with position X=" << x << " C=" << c;
    throw(err.str());
}

bool Board::isMoveValid(const PiecePtr& piece, const CellPtr& dst)
{
    return false;
}

void Board::getPossibleMoves(const CellPtr& pos, std::vector<CellPtr>& moves)
{
    
}

void Board::getMoveSteps(const CellPtr& start, const CellPtr& finish, std::vector<CellPtr>& steps)
{
    
}
