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
    
    void clear();
    
    CellPtr& getCell(unsigned c, unsigned x);   
    
    void placePiece(const PiecePtr& piece, unsigned c, unsigned x);
          
    bool isMoveValid(const CellPtr& finish);
    
    void getPossibleMoves(const PiecePtr& piece, std::vector<CellPtr>& moves);
    
    void getMoveSteps(const CellPtr& finish, std::vector<CellPtr>& steps);
    
private:
    
    void unmarkAll();
    
    void markNeibours(const CellPtr& cell);
    
    void markIfVacant(const CellPtr& prev, const CellPtr& cell);
    
    unsigned getRightPos(unsigned pos);

    unsigned getLeftPos(unsigned pos);
    
    std::vector<CellPtr> mCells; // all the cells on board
    
    std::vector<CellPtr> mPossibleMoves;
    
    PiecePtr mActivePiece;
};

#endif /*BOARD_H_*/
