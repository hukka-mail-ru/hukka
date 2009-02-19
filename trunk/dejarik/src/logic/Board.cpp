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

void Board::placePiece(const PiecePtr& piece, const CellPtr& cell)
{
    TRY_BEGINS;
    
    piece->cell = cell;
    cell->piece = piece;    
    
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


bool Board::isMoveValid(const CellPtr& cell)
{
    TRY_BEGINS;
    
    for(unsigned i=0; i<mPossibleMoves.size(); ++i)
    {
        if(mPossibleMoves[i]->c == cell->c && mPossibleMoves[i]->r == cell->r)
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

void Board::getPossiblePushes(const PiecePtr& piece, vector<CellPtr>& pushes)
{
    TRY_BEGINS;
    
    unsigned temp = piece->moveRating; // just memorize
    piece->moveRating = 1;
    
    getPossibleMoves(piece, pushes);
    
    piece->moveRating = temp; // restore
    
    TRY_RETHROW;
}


void Board::getPossibleMoves(const PiecePtr& piece, vector<CellPtr>& moves)
{
    TRY_BEGINS;
    
    mActivePiece = piece;
    
    unmarkAll();
    
    //  mark possible moves from the start
    markNeibours(POSSIBLE_MOVES, 1, piece->cell);
    
    // memorize them
    moves.clear();
    for(unsigned i=0; i<mCells.size(); i++)
    {
        if(mCells[i]->mark == 1)
            moves.push_back(mCells[i]);
    }

       /*
    cout << "=== mark1 ====" << endl;
    for(unsigned i=0; i<moves.size(); ++i)
        cout << "move[" << i << "]= " <<  moves[i]->c  << "." <<  moves[i]->x << endl;
    cout << "===" << endl;*/
    
    // mark possible moves for others
    for(unsigned step=2; step <= piece->moveRating; step++)
    {
        for(unsigned i=0; i<moves.size(); ++i)
        {
            markNeibours(POSSIBLE_MOVES, step, moves[i]);
       
            /*
            cout << "=== mCells" << step << " ==== afrer markNeibours " <<  moves[i]->c  << "." <<  moves[i]->x << endl;
            for(unsigned i=0; i<mCells.size(); ++i)
                cout << "cell[" <<  mCells[i]->c  << "." <<  mCells[i]->x << "] " << mCells[i]->mark << endl;
            cout << "=========" << endl;*/
        } 
        

        
        // memorize them
        moves.clear();
        for(unsigned i=0; i<mCells.size(); i++)
        {
            if(mCells[i]->mark == step)
                moves.push_back(mCells[i]);
        }
        
        /*
        cout << "=== mark" << step << " ====" << endl;
        for(unsigned i=0; i<moves.size(); ++i)
            cout << "move[" << i << "]= " <<  moves[i]->c  << "." <<  moves[i]->x << endl;
        cout << "=========" << endl;*/
    }
    
    
    mPossibleMoves = moves;
        

       
    TRY_RETHROW;    
}


void Board::getPossibleTargets(const PiecePtr& piece, std::vector<CellPtr>& targets)
{
    TRY_BEGINS;
    
    unmarkAll();
    
    const unsigned marked = 1;
    
    //  mark possible moves from the start
    markNeibours(POSSIBLE_TARGETS, marked, piece->cell);
    
    // memorize them
    for(unsigned i=0; i<mCells.size(); i++)
    {
        if(mCells[i]->mark == marked)
            targets.push_back(mCells[i]);
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
    
    if(whatToMark == POSSIBLE_MOVES) // mark vacant cells
    {
        if(!cell->piece)
        {
            cell->mark = step;
            cell->prev = prev;
        }
    }
    else if(whatToMark == POSSIBLE_TARGETS)
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
    for(unsigned i = 0; i < mCells.size(); i++)
    {
        mCells[i]->selected = SEL_NONE;
    }
    
    cell->selected = SEL_CLICKED;
}
