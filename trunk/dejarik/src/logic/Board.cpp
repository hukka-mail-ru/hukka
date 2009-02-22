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

void Board::killPiece(PiecePtr piece)
{
    TRY_BEGINS;
    
    CellPtr cell = piece->cell;
    
    piece->player->removePiece(piece);
    piece.reset();
    cell->piece.reset();
    
    assert(!piece);
    assert(!cell->piece);
    
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

void Board::definePossiblePushClicks(const PiecePtr& piece)
{
    TRY_BEGINS;
    
    mPossibleClicks.clear();
    
    getPossiblePushes(piece, mPossibleClicks);
        
    TRY_RETHROW;
}


void Board::definePossibleClicks(const PlayerPtr& player)
{
    TRY_BEGINS;
    
    mPossibleClicks.clear();
    
    PiecePtr activePiece = player->getActivePiece();
    
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
        getPossibleMoves(activePiece, moves);
        mPossibleClicks.insert(mPossibleClicks.end(), moves.begin(), moves.end());

        // + targets
        vector<CellPtr> targets;
        getPossibleTargets(activePiece, targets);
        mPossibleClicks.insert(mPossibleClicks.end(), targets.begin(), targets.end());
    }
    
    TRY_RETHROW;
}





void Board::getPossiblePushes(const PiecePtr& piece, vector<CellPtr>& pushes)
{
    TRY_BEGINS;
    
    unsigned temp = piece->moveRating; // just memorize
    piece->moveRating = 1;
    
    getPossibleMoves(piece, pushes);
    
    piece->moveRating = temp; // restore
    
    TRY_RETHROW;
}


void Board::getPossibleMoves(const PiecePtr& piece, vector<CellPtr>& possibleMoves)
{
    TRY_BEGINS;
    
    mActivePiece = piece;
    
    //unmarkAll();
    for(unsigned i=0; i<mCells.size(); ++i)
    {
        mCells[i]->mark = 0;
        mCells[i]->prev.reset();
    } 
    
    
    vector<CellPtr> moves;
    //  mark possible moves from the start
    markNeibours(MARK_POSSIBLE_MOVES, 1, piece->cell);
    
    // memorize them
    moves.clear();
    for(unsigned i=0; i<mCells.size(); i++)
    {
        if(mCells[i]->mark == 1)
        {
            moves.push_back(mCells[i]);
         //   mCells[i]->selected = SEL_POSSIBLE_MOVE;
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
              //  mCells[i]->selected = SEL_POSSIBLE_MOVE;
            }
        }
    }
    
    
    for(unsigned i=0; i<moves.size(); i++)
    {
        if(moves[i]->mark == piece->moveRating)
        {
            possibleMoves.push_back(moves[i]);
            moves[i]->selected = SEL_POSSIBLE_MOVE;
        }
    }
    
          
    TRY_RETHROW;    
}


void Board::getPossibleTargets(const PiecePtr& piece, std::vector<CellPtr>& targets)
{
    TRY_BEGINS;
    
    //unmarkAll();
    for(unsigned i=0; i<mCells.size(); ++i)
    {
        mCells[i]->mark = 0;
    } 
    
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

bool isInPrevious(const CellPtr& start)
{
    CellPtr cell = start; // copy
    
    unsigned i = 0;
    for(;;)
    {        
        if(!cell->prev)
            return false;
        
        cell = cell->prev;
        
        if(cell == start)
            return true;
        
        i++;
        assert(i<4); // 3 is the maximum moves quantity
    }  
}

void Board::mark(WhatToMark whatToMark, unsigned step, const CellPtr& prev, const CellPtr& cell)
{
    TRY_BEGINS;
    
    if(whatToMark == MARK_POSSIBLE_MOVES) 
    {
        // mark vacant cells, but:
        // can't move finishing on the start position
        // can't move through a cell twice
        if(!cell->piece && !isInPrevious(cell))
        {            
            cell->mark = step;
            
            if(!cell->prev) // the previous cell must be set ONCE
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
    
    unsigned i = 0;
    for(;;)
    {
        /*
        if(cell->prev)
        cout << "Cell: " << cell->c << "." << cell->r << 
         "   prev: " << cell->prev->c << "." << cell->prev->r << endl;
        else
        cout << "Cell: " << cell->c << "." << cell->r <<  " NO PREV " << endl;
        */
        
        if(!cell->prev)
            break;
        
        steps.insert(steps.begin(), cell); // push_front
        
        cell = cell->prev;
        
        i++;
        assert(i<4); // 3 is the maximum moves quantity
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
