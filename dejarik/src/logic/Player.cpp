#include "Player.h"

#include <cstdlib> 
#include <ctime> 
#include <sstream>

using namespace std;

#ifdef UNIT_TESTS
bool TestMakePush = false;
bool TestMakeCounterPush = false;
#endif

void Player::addPiece(const PiecePtr& piece)
{
    mPieces.push_back(piece);
}

void Player::removePiece(const PiecePtr& piece)
{
    unsigned init = mPieces.size();

    mPieces.erase(remove(mPieces.begin(), mPieces.end(), piece), mPieces.end());
    
    assert(mPieces.size() == init - 1);
}

unsigned Player::howManyPieces()
{
    return mPieces.size();
}


bool Player::makeTurn(unsigned c, unsigned x, TurnStage turnStage, BattleResult& battleResult) 
{
    TRY_BEGINS;
    
    // Define the cell: empty ? ally ? enemy ?
    CellPtr cell = mBoard->getCell(c, x);
    PiecePtr piece = cell->piece;
    
    // can't start from an empty cell
    if(!piece && turnStage == TURN_SELECTION)
    {
        return false;
    }
    
    // can't start from an enemy piece
    if(piece && piece->player.get() != this && turnStage == TURN_SELECTION)
    {
        return false;
    }
    
    // if clicked on ally, we must do TURN_SELECTION, even if we obtained TURN_ACTION
    if(piece && piece->player.get() == this && turnStage == TURN_ACTION)
    {
        turnStage = TURN_SELECTION;
    }
    
    // on TURN_SELECTION:
    if(turnStage == TURN_SELECTION)
    {
        mActivePiece = piece;
        
        vector<CellPtr> possibleMoves;
        mBoard->getPossibleMoves(piece, possibleMoves);
        
        // GUI::showPossibleMoves() ... ?        
    }
    else if(turnStage == TURN_ACTION)
    {
        if(!piece) // move to cell
        {
            return movePiece(mActivePiece, c, x);
        }
        else // attack enemy piece
        {
            battleResult = attackEnimy(mActivePiece, piece); 
            return true;
        }
    }
    
    TRY_RETHROW;
    
    return true;
}


bool Player::makePush(unsigned c, unsigned x)
{    
    TRY_BEGINS;
    
    return movePiece(mBoard->getActivePiece(), c, x);
    
    TRY_RETHROW;
}


BattleResult Player::attackEnimy(const PiecePtr& myPiece, const PiecePtr& enemyPiece)
{
    TRY_BEGINS;
    
    // an assurance
    assert(myPiece->player != enemyPiece->player);
    
    BattleResult res = getBattleResult(myPiece->attackRating, enemyPiece->defenceRating);
    
    if(res == RES_KILL)
    {
        mBoard->killPiece(const_cast<PiecePtr&>(enemyPiece));
    }
    else if(res == RES_COUNTER_KILL)
    {
        mBoard->killPiece(const_cast<PiecePtr&>(myPiece));
    }
    else if(res == RES_PUSH)
    {
        vector<CellPtr> possiblePushes;
        mBoard->getPossiblePushes(enemyPiece, possiblePushes);
        // GUI::showPossibleMoves() ... ?   
    }
    else if(res == RES_COUNTER_PUSH)
    {
        vector<CellPtr> possiblePushes;
        mBoard->getPossiblePushes(myPiece, possiblePushes);
        // GUI::showPossibleMoves() ... ?   
    }
    
    return res;
    
    TRY_RETHROW;
}



BattleResult Player::getBattleResult(unsigned attackRating, unsigned defenceRating)
{
#ifdef UNIT_TESTS
    if(TestMakePush)
        return RES_PUSH;

    if(TestMakeCounterPush)
        return RES_COUNTER_PUSH;
#endif
    
    
    srand((unsigned)time(0)); 
    
    unsigned attack = 0;
    for(unsigned i = 0; i < attackRating; i++)
    {
        attack += (rand()%6)+1; // random number 1..6 (like a dice) 
    }
    
    unsigned defence = 0;
    for(unsigned i = 0; i < defenceRating; i++)
    {
        defence += (rand()%6)+1; // random number 1..6 (like a dice) 
    }
    
    
    if(attack > defence)
    {
        if(attack >= defence + 7) // if Attack beats Defense by 7 or more then Kill
        {
            return RES_KILL;
        }
        else if(attack <= defence + 6) // if Attack beats Defense by 6 or less then Push
        {
            return RES_PUSH;
        }
    }
    else if (attack == defence) //  if a draw then Counter-Push
    {
        return RES_COUNTER_PUSH;
    }
    else if(defence > attack)
    {
        if(defence >= attack + 7) // if Defense beats attack by 7 or less then Counter-Kill
        {
            return RES_COUNTER_KILL;
        }
        else if(defence <= attack + 6) //  if Defense beats Attack by 6 or less then Counter-Push
        {
            return RES_COUNTER_PUSH;
        }
    }
    
    stringstream err;
    err << __FUNCTION__ << " Battle result is undefined: attack=" << attack << ", defence=" << defence;
    throw(err.str());
    
    return RES_NO_BATTLE;
}


bool Player::movePiece(const PiecePtr& piece, unsigned c, unsigned x) 
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
        mBoard->placePiece(piece, steps[i]);
      //  cout << "movePiece (" << piece->getName() << ")to " <<  steps[i]->c << "." << steps[i]->x << endl;
    }
    
    TRY_RETHROW;
    
    return true;
}
