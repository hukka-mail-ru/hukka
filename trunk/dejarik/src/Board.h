#ifndef BOARD_H_
#define BOARD_H_

#include <vector>
#include "Cell.h"
#include "Macros.h"
#include "Piece.h"

#define CIRCLE 12 // cells in a circle

CLASSPTR(Piece);
CLASSPTR(Cell);

class Board // only one board in game
{
    friend class TestBoard;
    friend class TestPlayer;
    
public:
    
    // on Game start  
    Board();
    void placePiece(const PiecePtr& piece, unsigned c, unsigned x);
    void placePiece(const PiecePtr& piece, const CellPtr& cell);
    
    // on Game over
    void clear();
    
    // on user click
    CellPtr& getCell(unsigned c, unsigned x);   

    // on user click1: (activate a piece => show possible moves and targets)
    void getPossibleMoves(const PiecePtr& piece, std::vector<CellPtr>& moves);
    void getPossibleTargets(const PiecePtr& piece, std::vector<CellPtr>& targets);

    // on user click2: (move the piece or attack a partner's piece)
    bool isMoveValid(unsigned c, unsigned x);
    void getMoveSteps(unsigned c, unsigned x, std::vector<CellPtr>& steps);
    
private:
    
    void unmarkAll();
    
    enum WhatToMark
    {
        POSSIBLE_MOVES,
        POSSIBLE_TARGETS
    };
    
    void markNeibours(WhatToMark whatToMark, unsigned step, const CellPtr& cell);       
    void mark(WhatToMark whatToMark, unsigned step, const CellPtr& prev, const CellPtr& cell);
    
    unsigned getRightPos(unsigned pos);
    unsigned getLeftPos(unsigned pos);
    
    std::vector<CellPtr> mCells; // all the cells on board
    
    std::vector<CellPtr> mPossibleMoves;
    
    PiecePtr mActivePiece;
};

CLASSPTR(Board);

#endif /*BOARD_H_*/
