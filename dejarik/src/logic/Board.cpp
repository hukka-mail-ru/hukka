#include <sstream>
#include <iostream>
#include "Board.h"


using namespace std;

Board::Board()
{
    CellPtr cell0 (new Cell(0,0));
    mCells.push_back(cell0);
    
    for(unsigned i=0; i<RADIUSES; i++)
    {
        CellPtr cell1 (new Cell(1,i));
        mCells.push_back(cell1);
        
        CellPtr cell2 (new Cell(2,i));
        mCells.push_back(cell2);
    }       
}

void Board::getInitialCells(vector<CellPtr>& cells)
{
    TRY_BEGINS;
    
    cells.push_back(getCell(2,0));
    cells.push_back(getCell(2,1));
    cells.push_back(getCell(2,2));
    cells.push_back(getCell(2,3));
    cells.push_back(getCell(2,6));
    cells.push_back(getCell(2,7));
    cells.push_back(getCell(2,8));
    cells.push_back(getCell(2,9));
    
    TRY_RETHROW;
}

void Board::clear()
{
    for(unsigned i=0; i<mCells.size(); ++i)
    {
        mCells[i]->piece.reset();
        mCells[i]->mark = 0;
        mCells[i]->prev.reset();
    } 
}

void Board::unmarkAll()
{
    for(unsigned i=0; i<mCells.size(); ++i)
    {
        mCells[i]->mark = 0;
        mCells[i]->prev.reset();
    } 
}

// piece <---> cell
void Board::placePiece(const PiecePtr& piece, unsigned c, unsigned x)
{
    TRY_BEGINS;
    
    CellPtr cell = getCell(c, x);
    
    placePiece(piece, cell);
    
    TRY_RETHROW;
}

void Board::placePiece(const PiecePtr& piece, const CellPtr& newcell)
{
    TRY_BEGINS;
    
    CellPtr oldcell = piece->cell;
    
    if(oldcell)
    {
        assert(oldcell->piece);
        
        newcell->piece.swap(oldcell->piece);
        
        assert(!oldcell->piece);
        assert(newcell->piece);
    }
    else
    {
        newcell->piece = piece;
    }

    piece->cell = newcell;
    
    TRY_RETHROW;
}

void Board::distribute(const PiecePtr& piece, const PlayerPtr& player)
{
    TRY_BEGINS;
    
    player->addPiece(piece);
    piece->player = player;
    
    TRY_RETHROW;
}

void Board::killPiece(PiecePtr& piece)
{
    TRY_BEGINS;
    
    piece->player->removePiece(piece);
    piece.reset();
    
    TRY_RETHROW;
}



CellPtr& Board::getCell(unsigned c, unsigned r)
{
    for(unsigned i=0; i<mCells.size(); ++i)
    {
        if(mCells[i]->c == c && mCells[i]->r == r)
        {
            return mCells[i];
        }
    }
    
    stringstream err;
    err << "Board::getCell. No cell with position C=" << c << ", R=" << r;
    throw(err.str());
}


bool Board::isClickValid(const CellPtr& cell)
{
    TRY_BEGINS;
    
    for(unsigned i=0; i<mPossibleClicks.size(); ++i)
    {
        if(mPossibleClicks[i] == cell)
        {
            return true;
        }
    }   
    
    TRY_RETHROW;        
    return false;
}


const PiecePtr& Board::getActivePiece()
{
    return mActivePiece;
}


void Board::definePossibleClicks(const PlayerPtr& player, bool push)
{
    TRY_BEGINS;
    
    mPossibleClicks.clear();
    
    PiecePtr activePiece = player->getActivePiece();
    
    if(push) // (battleRes == RES_PUSH || battleRes == RES_COUNTER_PUSH)
    {
        if(activePiece)
        {
            vector<CellPtr> pushes;
            definePossibleMoves(activePiece, pushes);
            mPossibleClicks = pushes;
        }
        
        return;
    }
    
    // all player's pieces   
    for(unsigned i=0; i<mCells.size(); ++i)
    {
        if(mCells[i]->piece && mCells[i]->piece->player == player)
            mPossibleClicks.push_back(mCells[i]);
    }
    
    if(activePiece)
    {
        // + moves
        vector<CellPtr> moves;
        definePossibleMoves(activePiece, moves);
        mPossibleClicks.insert(mPossibleClicks.end(), moves.begin(), moves.end());

        // + targets
        vector<CellPtr> targets;
        definePossibleMoves(activePiece, targets);
        mPossibleClicks.insert(mPossibleClicks.end(), targets.begin(), targets.end());
    }
    
    TRY_RETHROW;
}





void Board::definePossiblePushes(const PiecePtr& piece, vector<CellPtr>& pushes)
{
    TRY_BEGINS;
    
    unsigned temp = piece->moveRating; // just memorize
    piece->moveRating = 1;
    
    definePossibleMoves(piece, pushes);
    
    piece->moveRating = temp; // restore
    
    TRY_RETHROW;
}


void Board::definePossibleMoves(const PiecePtr& piece, vector<CellPtr>& moves)
{
    TRY_BEGINS;
    
    mActivePiece = piece;
    
    unmarkAll();
    
    //  mark possible moves from the start
    markNeibours(MARK_POSSIBLE_MOVES, 1, piece->cell);
    
    // memorize them
    moves.clear();
    for(unsigned i=0; i<mCells.size(); i++)
    {
        if(mCells[i]->mark == 1)
        {
            moves.push_back(mCells[i]);
            mCells[i]->selected = SEL_POSSIBLE_MOVE;
        }
    }
    
    // mark possible moves for others
    for(unsigned step=2; step <= piece->moveRating; step++)
    {
        for(unsigned i=0; i<moves.size(); ++i)
        {
            markNeibours(MARK_POSSIBLE_MOVES, step, moves[i]);
        } 
        
        // memorize them
        moves.clear();
        for(unsigned i=0; i<mCells.size(); i++)
        {
            if(mCells[i]->mark == step)
            {
                moves.push_back(mCells[i]);
                mCells[i]->selected = SEL_POSSIBLE_MOVE;
            }
        }
    }
          
    TRY_RETHROW;    
}


void Board::definePossibleTargets(const PiecePtr& piece, std::vector<CellPtr>& targets)
{
    TRY_BEGINS;
    
    unmarkAll();
    
    const unsigned marked = 1;
    
    //  mark possible moves from the start
    markNeibours(MARK_POSSIBLE_TARGETS, marked, piece->cell);
    
    // memorize them
    for(unsigned i=0; i<mCells.size(); i++)
    {
        if(mCells[i]->mark == marked)
        {
            targets.push_back(mCells[i]);
            mCells[i]->selected = SEL_POSSIBLE_TARGET;
        }
    }
    
    TRY_RETHROW;    
}


void Board::markNeibours(WhatToMark whatToMark, unsigned step, const CellPtr& cell)
{
    TRY_BEGINS;
    
    if(cell->c == 0)
    {
        for(unsigned i=0; i<RADIUSES; ++i)
        {
            mark(whatToMark, step, cell, getCell(1, i));
        }
    }
    else if (cell->c == 1)
    {
        mark(whatToMark, step, cell, getCell(0, 0));
        mark(whatToMark, step, cell, getCell(1, getRightPos(cell->r)));
        mark(whatToMark, step, cell, getCell(1, getLeftPos(cell->r)));
        mark(whatToMark, step, cell, getCell(2, cell->r));
        
        //cout << "markNeibours[" << step << "]= " <<  2 << "." <<  cell->x << endl;
    }
    else if (cell->c == 2)
    {
        mark(whatToMark, step, cell, getCell(2, getRightPos(cell->r)));
        mark(whatToMark, step, cell, getCell(2, getLeftPos(cell->r)));
        mark(whatToMark, step, cell, getCell(1, cell->r));
    }
    else
    {
        throw string("Invalid circle number");
    }
         
    TRY_RETHROW;       
}

void Board::mark(WhatToMark whatToMark, unsigned step, const CellPtr& prev, const CellPtr& cell)
{
    TRY_BEGINS;
    
    if(whatToMark == MARK_POSSIBLE_MOVES) // mark vacant cells
    {
        if(!cell->piece)
        {
            cell->mark = step;
            cell->prev = prev;
        }
    }
    else if(whatToMark == MARK_POSSIBLE_TARGETS)
    {
        if(cell->piece && (cell->piece->player != prev->piece->player)) // mark opponent's pieces
        {
            cell->mark = step;
        }
    }
    else
    {
        throw string("Invalid 'whatToMark' parameter");
    }
    
    TRY_RETHROW;     
}

unsigned Board::getRightPos(unsigned pos)
{
    if(pos == RADIUSES - 1)
        return 0;
    else
        return ++pos;
}

unsigned Board::getLeftPos(unsigned pos)
{
    if(pos == 0)
        return RADIUSES - 1;
    else
        return --pos;
}




void Board::getMoveSteps(const CellPtr& start, std::vector<CellPtr>& steps)
{
    TRY_BEGINS;
    
    CellPtr cell = start;
    
    for(;;)
    {
        if(!cell->prev)
            break;
        
        steps.insert(steps.begin(), cell); // push_front
        
        cell = cell->prev;
    }

    
    TRY_RETHROW;  
}

void Board::getCells(vector<CellPtr>& cells)
{
    cells = mCells;    
}

void Board::selectClickedCell(const CellPtr& cell)
{   
    cell->selected = SEL_CLICKED;
}

void Board::deselectAll()
{
    for(unsigned i = 0; i < mCells.size(); i++)
    {
        mCells[i]->selected = SEL_NONE;
    }
}

void Board::selectAll(const PlayerPtr& player)
{
    for(unsigned i = 0; i < mCells.size(); i++)
    {
        if(mCells[i]->piece && mCells[i]->piece->player == player)
            mCells[i]->selected = SEL_POSSIBLE_MOVE;
    }    
}
