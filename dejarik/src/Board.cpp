#include <sstream>
#include <iostream>
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
    mCells.push_back(cell0);
    
    for(unsigned i=0; i<CIRCLE; i++)
    {
        CellPtr cell1 (new Cell(1,i));
        mCells.push_back(cell1);
        
        CellPtr cell2 (new Cell(2,i));
        mCells.push_back(cell2);
    }       
}

void Board::clear()
{
    for(unsigned i=0; i<mCells.size(); ++i)
    {
        mCells[i]->piece.reset();
        mCells[i]->mark = false;
        mCells[i]->prev.reset();
    } 
}

void Board::unmarkAll()
{
    for(unsigned i=0; i<mCells.size(); ++i)
    {
        mCells[i]->mark = false;
        mCells[i]->prev.reset();
    } 
}


CellPtr& Board::getCell(unsigned c, unsigned x)
{
    for(unsigned i=0; i<mCells.size(); ++i)
    {
        if(mCells[i]->c == c && mCells[i]->x == x)
        {
            return mCells[i];
        }
    }
    
    stringstream err;
    err << "Board::getCell. No cell with position X=" << x << " C=" << c;
    throw(err.str());
}


bool Board::isMoveValid(const CellPtr& finish)
{
    TRY_BEGINS;
    
    for(unsigned i=0; i<mPossibleMoves.size(); ++i)
    {
        if(mPossibleMoves[i]->c == finish->c && mPossibleMoves[i]->x == finish->x)
        {
            return true;
        }
    }
    
    RETHROW("Board::isMoveValid");    
    
    return false;
}


void Board::getPossibleMoves(const PiecePtr& piece, vector<CellPtr>& moves)
{
    TRY_BEGINS;
    
    unmarkAll();
    
    //  mark possible moves from the start
    markNeibours(piece->getPosition());
    
    // memorize them
    for(unsigned i=0; i<mCells.size(); i++)
    {
        if(mCells[i]->mark)
            moves.push_back(mCells[i]);
    }

       /*
    for(unsigned i=0; i<moves.size(); ++i)
        cout << "move[" << i << "]= " <<  moves[i]->c  << "." <<  moves[i]->x << endl;
    cout << "===" << endl;*/
    
    // mark possible moves for others
    for(unsigned move=0; move < piece->getMoveRating()-1; move++)
    {
        for(unsigned i=0; i<moves.size(); ++i)
        {
            markNeibours(moves[i]);
        } 
        
        // memorize them
        for(unsigned i=0; i<mCells.size(); i++)
        {
            if(mCells[i]->mark)
                moves.push_back(mCells[i]);
        }
    }
    
    mActivePiece = piece;
    mPossibleMoves = moves;
    /*    
    for(unsigned i=0; i<moves.size(); ++i)
        cout << "move[" << i << "]= " <<  moves[i]->c  << "." <<  moves[i]->x << endl;
    cout << "=========" << endl;*/
        
    RETHROW("Board::getPossibleMoves");    
}


void Board::markNeibours(const CellPtr& cell)
{
    TRY_BEGINS;
    
    if(cell->c == 0)
    {
        for(unsigned i=0; i<CIRCLE; ++i)
        {
            markIfVacant(cell, getCell(1, i));
        }
    }
    else if (cell->c == 1)
    {
        markIfVacant(cell, getCell(0, 0));
        markIfVacant(cell, getCell(1, getRightPos(cell->x)));
        markIfVacant(cell, getCell(1, getLeftPos(cell->x)));
        markIfVacant(cell, getCell(2, cell->x));
    }
    else if (cell->c == 2)
    {
        markIfVacant(cell, getCell(2, getRightPos(cell->x)));
        markIfVacant(cell, getCell(2, getLeftPos(cell->x)));
        markIfVacant(cell, getCell(1, cell->x));
    }
    else
    {
        throw string("Board::markNeibours. Invalid circle number");
    }
         
    RETHROW("Board::markNeibours");       
}

void Board::markIfVacant(const CellPtr& prev, const CellPtr& cell)
{
    TRY_BEGINS;
    
    if(!cell->piece && !cell->mark)
    {
        cell->mark = true;
        cell->prev = prev;
    }
    
    RETHROW("Board::markIfVacant");     
}

unsigned Board::getRightPos(unsigned pos)
{
    if(pos == CIRCLE - 1)
        return 0;
    else
        return ++pos;
}

unsigned Board::getLeftPos(unsigned pos)
{
    if(pos == 0)
        return CIRCLE - 1;
    else
        return --pos;
}




void Board::getMoveSteps(const CellPtr& finish, std::vector<CellPtr>& steps)
{
    TRY_BEGINS;
    
  //  vector<CellPtr> moves;    
  //  getPossibleMoves(mActivePiece->getPosition(), mActivePiece->getMoveRating(), moves);
    
    
    
    RETHROW("Board::getMoveSteps");  
}
