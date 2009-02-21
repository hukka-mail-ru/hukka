#ifndef BOARD_H_
#define BOARD_H_

#include <vector>
#include "Player.h"
#include "Cell.h"
#include "../common/Macros.h"
#include "Piece.h"


#define CIRCLES 2 
#define RADIUSES 12 // cells in a circle

CLASSPTR(Board);
CLASSPTR(Piece);
CLASSPTR(Player);
CLASSPTR(Cell);


class Board // only one board in game
{
    friend class TestBoard;
    friend class TestPlayer;
    friend class TestGame;
    
public:
    
    // on Game start  
    Board();
    
    void getInitialCells(std::vector<CellPtr>& cells);
    
    void placePiece(const PiecePtr& piece, unsigned c, unsigned r);
    void placePiece(const PiecePtr& piece, const CellPtr& cell);
    
    void distribute(const PiecePtr& piece, const PlayerPtr& player);
    
    // on Game over
    void clear();
    
    // on user click
    void getCells(std::vector<CellPtr>& cells); // get all cells
    void deselectAll();
    void selectAll(const PlayerPtr& player);
    void selectClickedCell(const CellPtr& cell);

    // on user click1: (activate a piece => show possible moves and targets)
    void definePossibleClicks(const PlayerPtr& player, bool push);

    // on user click2: (move the piece OR attack a partner's piece OR push a piece)
    bool isClickValid(const CellPtr& cell);
    void getMoveSteps(const CellPtr& start, std::vector<CellPtr>& steps);
    
    // on a kill
    void killPiece(PiecePtr piece);
    
    // on a push
    const PiecePtr& getActivePiece();
    
private:
    
    CellPtr& getCell(unsigned c, unsigned r);      

    void unmarkAll();
    
    enum WhatToMark
    {
        MARK_POSSIBLE_MOVES,
        MARK_POSSIBLE_TARGETS
    };
    
    void markNeibours(WhatToMark whatToMark, unsigned step, const CellPtr& cell);       
    void mark(WhatToMark whatToMark, unsigned step, const CellPtr& prev, const CellPtr& cell);

    void getPossibleMoves(const PiecePtr& piece, std::vector<CellPtr>& moves);
    void getPossibleTargets(const PiecePtr& piece, std::vector<CellPtr>& targets);
    void getPossiblePushes(const PiecePtr& piece, std::vector<CellPtr>& pushes);

    unsigned getRightPos(unsigned pos);
    unsigned getLeftPos(unsigned pos);
    
    std::vector<CellPtr> mCells; // all the cells on board
    
    std::vector<CellPtr> mPossibleClicks;
    
    PiecePtr mActivePiece;
};


#endif /*BOARD_H_*/
