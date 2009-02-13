#include "Player.h"

using namespace std;

/* 
     Define the cell: empty ? ally ? enemy ?
      
    if clicked on ally, we must do TURN_START, even we are obtained TURN_FINISH
    
    on TURN_START:
       select mActivePiece
       player gets possible moves 
    
    on TURN_FINISH:
      player moves mActivePiece 
        step-by-step, in cycle, mActivePiece.setPosition
      or attacks an enemy piece:
        enemy = get enemy piece 
        computeBattleRusult
        res. can be: enemy.push or enemy.kill
        res. can be: mActivePiece.push or mActivePiece.kill
    */
bool Player::makeTurn(unsigned c, unsigned x, TurnStage turnStage) 
{
    TRY_BEGINS;
    
    // Define the cell: empty ? ally ? enemy ?
    CellPtr cell = mBoard->getCell(c, x);
    PiecePtr piece = cell->piece;
    
    // can't start from an empty cell
    if(!piece && turnStage == TURN_START)
    {
        return false;
    }
    
    // can't start from an enemy piece
    if(piece && piece->getPlayer().get() != this && turnStage == TURN_START)
    {
        return false;
    }
    
    // if clicked on ally, we must do TURN_START, even if we obtained TURN_FINISH
    if(piece && piece->getPlayer().get() == this && turnStage == TURN_FINISH)
    {
        turnStage = TURN_START;
    }
    
    // on TURN_START:
    if(turnStage == TURN_START)
    {
        mActivePiece = piece;
        
        vector<CellPtr> possibleMoves;
        mBoard->getPossibleMoves(piece, possibleMoves);
    }
    else if(turnStage == TURN_FINISH)
    {
        if(!piece) // move to cell
        {
            return moveActivePiece(c, x);
        }
        else // attack enemy piece
        {
            return attackEnimy(mActivePiece, piece); // here 'piece' definitely means 'enemy piece'
        }
    }
    
    TRY_RETHROW;
    
    return true;

}


bool Player::moveActivePiece(unsigned c, unsigned x) 
{
    TRY_BEGINS;
    
    if(!mBoard->isMoveValid(c, x))
    {
        return false;
    }
    
    vector<CellPtr> steps;
    mBoard->getMoveSteps(c, x, steps);
    
    for(unsigned i = 0; i<steps.size(); i++)
    {
        mBoard->placePiece(mActivePiece, steps[i]);
        cout << "moveActivePiece (" << mActivePiece->getName() << ")to " <<  steps[i]->c << "." << steps[i]->x << endl;
    }
    
    TRY_RETHROW;
    
    return true;
}
