#ifndef BOARD_H_
#define BOARD_H_

#include <vector>
#include "Cell.h"

#define CIRCLE 12 // cells in a circle

class Board // only one board in game
{
    friend class DejaricTest;
    
public:
    
    // on Game start  
    Board();
    void placePiece(const PiecePtr& piece, unsigned c, unsigned x);
    
    // on Game over
    void clear();

    // on user click1: (activate a piece => show possible moves and targets)
    void getPossibleMoves(const PiecePtr& piece, std::vector<CellPtr>& moves);
    void getPossibleTargets(const PiecePtr& piece, std::vector<CellPtr>& targets);

    // on user click2: (move the piece or attack a partner's piece)
    bool isMoveValid(unsigned c, unsigned x);
    void getMoveSteps(unsigned c, unsigned x, std::vector<CellPtr>& steps);
    
private:

    CellPtr& getCell(unsigned c, unsigned x);   
    
    void unmarkAll();
    
    enum WhatToMark
    {
        POSSIBLE_MOVES,
        POSSIBLE_TARGETS
    };
    
    void markNeibours(WhatToMark whatToMark, const CellPtr& cell);       
    void mark(WhatToMark whatToMark, const CellPtr& prev, const CellPtr& cell);
    
    unsigned getRightPos(unsigned pos);
    unsigned getLeftPos(unsigned pos);
    
    std::vector<CellPtr> mCells; // all the cells on board
    
    std::vector<CellPtr> mPossibleMoves;
    
    PiecePtr mActivePiece;
};

#endif /*BOARD_H_*/
