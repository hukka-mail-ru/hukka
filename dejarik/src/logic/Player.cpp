#include "Player.h"

#include <cstdlib> 
//#include <ctime> 
#include <sstream>

using namespace std;

#ifdef WIN_BUILD
time_t time( time_t *inTT )
{
SYSTEMTIME sysTimeStruct;
FILETIME fTime;
ULARGE_INTEGER int64time;
time_t locTT = 0;

if ( inTT == NULL ) {
inTT = &locTT;
}

GetSystemTime( &sysTimeStruct );
if ( SystemTimeToFileTime( &sysTimeStruct, &fTime ) ) {
memcpy( &int64time, &fTime, sizeof( FILETIME ) );
/* Subtract the value for 1970-01-01 00:00 (UTC) */
int64time.QuadPart -= 0x19db1ded53e8000;
/* Convert to seconds. */
int64time.QuadPart /= 10000000;
*inTT = (time_t)int64time.QuadPart;
}
return *inTT;
} 
#endif

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


BattleResult Player::attackEnimy(const PiecePtr& enemyPiece)
{
    TRY_BEGINS;
    
    // an assurance
    assert(mActivePiece.lock()->player != enemyPiece->player);
    
    enemyPiece->player->setActivePiece(enemyPiece);
    
    BattleResult res = getBattleResult(mActivePiece.lock()->attackRating, enemyPiece->defenceRating);
    
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
    throw runtime_error(err.str());
    
    return RES_MOVE;
}


void Player::movePiece(const CellPtr& cell) 
{
    TRY_BEGINS;
       
    vector<CellPtr> steps;       
    mBoard->getMoveSteps(cell, steps);
    
    for(unsigned i = 0; i<steps.size(); i++)
    {
        mBoard->placePiece(mActivePiece.lock(), steps[i]);
      //  cout << "movePiece (" << piece->getName() << ")to " <<  steps[i]->c << "." << steps[i]->x << endl;
    }
    
    TRY_RETHROW;
}


PiecePtr Player::getActivePiece()
{
    return mActivePiece.lock();
}

void Player::setActivePiece(const PiecePtr& piece)
{
    mActivePiece = piece;
}

void Player::resetActivePiece()
{
    mActivePiece.reset();
}


void Player::setLeftMoves(unsigned moves)
{
    mLeftMoves = moves;
}

void Player::decrementLeftMoves()
{
    mLeftMoves--;
}

unsigned Player::getLeftMoves()
{
    return mLeftMoves;
}
