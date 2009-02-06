#include <sstream>
#include "Board.h"
#include "Cell.h"

using namespace std;

#define TRY_BEGINS \
try \
{   

#define RETHROW(LOG) \
} \
catch(string& err) \
{ \
    string str = "["; \
    str += LOG; \
    str += "]->"; \
    str += err; \
    throw str; \
}


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


void Board::markAll(bool val)
{
    for(unsigned i=0; i<cells.size(); ++i)
    {
        cells[i]->mark = val;
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
    err << "Board::getCell. No cell with position X=" << x << " C=" << c;
    throw(err.str());
}


bool Board::isMoveValid(const PiecePtr& piece, const CellPtr& finish)
{
    TRY_BEGINS;
    
    vector<CellPtr> moves;    
    getPossibleMoves(piece->getPosition(), piece->getMoveRating(), moves);
    
    for(unsigned i=0; i<moves.size(); ++i)
    {
        if(moves[i]->c == finish->c && moves[i]->x == finish->x)
        {
            return true;
        }
    }
    
    RETHROW("Board::isMoveValid");    
    
    return false;
}


void Board::getPossibleMoves(const CellPtr& start, unsigned max, vector<CellPtr>& moves)
{
    TRY_BEGINS;
    
    markAll(false);
    start->mark = true;
    
    for(unsigned i=0; i<max; i++)
    {
        for(unsigned i=0; i<cells.size(); ++i)
        {
            if(!cells[i]->mark)
                continue;
            
            markNeibours(cells[i]);
        } 
    }
        
    RETHROW("Board::getPossibleMoves");    
}


void Board::markNeibours(const CellPtr& cell)
{
    TRY_BEGINS;
    
    if(cell->c == 0)
    {
        for(unsigned i=0; i<CIRCLE; ++i)
        {
            markIfVacant(1, i);
        }
    }
    else if (cell->c == 1)
    {
        markIfVacant(0, 0);
        markIfVacant(1, getRightPos(cell->x));
        markIfVacant(1, getLeftPos(cell->x));
        markIfVacant(2, cell->x);
    }
    else if (cell->c == 2)
    {
        markIfVacant(2, getRightPos(cell->x));
        markIfVacant(2, getLeftPos(cell->x));
        markIfVacant(1, cell->x);
    }
    else
    {
        throw string("Board::markNeibours. Invalid circle number");
    }
         
    RETHROW("Board::markNeibours");       
}


unsigned Board::getRightPos(unsigned pos)
{
    if(pos == CIRCLE - 1)
        return 0;
    else
        return pos++;
}

unsigned Board::getLeftPos(unsigned pos)
{
    if(pos == 0)
        return CIRCLE - 1;
    else
        return pos--;
}


void Board::markIfVacant(unsigned c, unsigned x)
{
    TRY_BEGINS;
    
    CellPtr cell = getCell(c, x);
    
    if(cell->piece == NULL)
    {
        cell->mark = true;
    }
    
    RETHROW("Board::markIfVacant");     
}

void Board::getMoveSteps(const CellPtr& start, const CellPtr& finish, std::vector<CellPtr>& steps)
{
    
}
