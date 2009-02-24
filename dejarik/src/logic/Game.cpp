#include <vector>
#include <sstream>

#include "Game.h"

#define NUM_MOVES 2

using namespace std;

void Game::startup()
{
    TRY_BEGINS;
    
    mBoard = BoardPtr(new Board());
    
    // TODO ask for player name
    
    mPlayer1 = PlayerPtr(new Player("Player1", mBoard));
    mPlayer2 = PlayerPtr(new Player("Player2", mBoard));

    vector<PiecePtr> pieces;    
    pieces.push_back(PiecePtr(new Piece("Sarvip",     6, 6, 2)));
    pieces.push_back(PiecePtr(new Piece("Monnok",     6, 5, 3)));
    pieces.push_back(PiecePtr(new Piece("Ghhhk",      4, 3, 2)));
    pieces.push_back(PiecePtr(new Piece("Houjix",     4, 4, 1)));
    pieces.push_back(PiecePtr(new Piece("Strider",    2, 7, 3)));
    pieces.push_back(PiecePtr(new Piece("Ng'ok",      3, 8, 1)));
    pieces.push_back(PiecePtr(new Piece("K'lor'slug", 7, 3, 2)));
    pieces.push_back(PiecePtr(new Piece("Molator",    8, 2, 2)));
    
    vector<CellPtr> cells;
    mBoard->getInitialCells(cells);  
    const unsigned pieces_num = pieces.size();
           
    // randomly divide the pieces between the players, 
    // and place them on opposites sides of the board
    srand((unsigned)time(0));     
    
    for(unsigned i=0; i<pieces_num; i++)
    {
        unsigned rnd = (rand()%pieces.size()); // random 0.. pieces.size() - 1
        
        PlayerPtr player = (i < pieces_num/2) ? mPlayer1 : mPlayer2; 
        
        mBoard->placePiece(pieces[rnd], cells[i]);               
        mBoard->distribute(pieces[rnd], player);        
        
        pieces.erase(remove(pieces.begin(), pieces.end(), pieces[rnd]), pieces.end()); 
    }
    
    passTurn();
    
    
    TRY_RETHROW;
}


void Game::passTurn(BattleResult battleResult)
{
    TRY_BEGINS;
    
    if(!mActivePlayer)
    {
        mActivePlayer = mPlayer1;
    }
    else
    {    
        mActivePlayer = (mActivePlayer == mPlayer1) ? mPlayer2 : mPlayer1; // next player
    }
    
    // no active piece at the beginning
    // but if RES_COUNTER_PUSH the player (enemy) must have an active piece (ours)
    if(battleResult != RES_COUNTER_PUSH)
    {
        mActivePlayer->resetActivePiece();
    }
    
    if(mActivePlayer->getLeftMoves() == 0)
    {
        mActivePlayer->setLeftMoves(NUM_MOVES);
        mBoard->definePossibleClicks(mActivePlayer);
    }
    
    
    
    TRY_RETHROW;
}



    
BattleResult Game::onCellClick(const CellPtr& cell)
{
    TRY_BEGINS;
    
    BattleResult res = RES_NO_BATTLE; 
    static BattleResult prevBattleResult = RES_NO_BATTLE;
    mBoard->deselectAll();
    
    if(!mBoard->isClickValid(cell))
    {
        return res;
    }
    
    // clicked on empty cell
    if(!cell->piece) 
    {
        // common move
        mActivePlayer->movePiece(cell);
        mActivePlayer->decrementLeftMoves();
        
        // push: we need to redefine where our pieces are.
        if(prevBattleResult == RES_PUSH)
        {
            mActivePlayer->resetActivePiece();
            mBoard->definePossibleClicks(mActivePlayer);
        }
    }
    else 
    {
        // clicked on our piece
        if(cell->piece->player == mActivePlayer) 
        {
            mActivePlayer->setActivePiece(cell->piece);
            mBoard->definePossibleClicks(mActivePlayer);
            mBoard->selectClickedCell(cell);
        }
        
        // clicked on enemy's piece
        else 
        {
            PiecePtr myPiece = mActivePlayer->getActivePiece(); // "me" means Player1
            PiecePtr enemyPiece = cell->piece;                  // "enemy" means Player2
            
            res = mActivePlayer->attackEnimy(enemyPiece);
            
            if(res == RES_KILL)
            {
                mBoard->killPiece(const_cast<PiecePtr&>(enemyPiece));
                mActivePlayer->decrementLeftMoves();
                mActivePlayer->resetActivePiece();
                mBoard->definePossibleClicks(mActivePlayer);
            }
            else if(res == RES_COUNTER_KILL)
            {
                mBoard->killPiece(myPiece);
                mActivePlayer->decrementLeftMoves();
                mActivePlayer->resetActivePiece();
                mBoard->definePossibleClicks(mActivePlayer);
            }
            else if(res == RES_PUSH)
            {
                mBoard->definePossiblePushClicks(enemyPiece);                 
                mActivePlayer->setActivePiece(enemyPiece); // We now owns enemy's piece!
            }
            else if(res == RES_COUNTER_PUSH)
            {
                mActivePlayer->decrementLeftMoves();
                
                PlayerPtr enemy = (mActivePlayer == mPlayer1) ? mPlayer2 : mPlayer1; // next player
                mBoard->definePossiblePushClicks(myPiece);
                enemy->setLeftMoves(1);
                
                // enemy now owns our piece!
                enemy->setActivePiece(myPiece);
                
                passTurn(RES_COUNTER_PUSH);
                
                prevBattleResult = res;
                return res;
            }
        }
    }
    
    if(mActivePlayer->getLeftMoves() == 0)
    {
        passTurn();
    }

    prevBattleResult = res;
    return res;
    
    TRY_RETHROW;
    
}
    
bool Game::checkVictory(PlayerPtr& vinner)
{
    TRY_BEGINS;
    
    if(mPlayer1->howManyPieces() == 0)
    {
        vinner = mPlayer2;
        return true;
    }
    else if(mPlayer2->howManyPieces() == 0)
    {
        vinner = mPlayer1;
        return true;
    }
    
    TRY_RETHROW;
    
    return false;
}

const BoardPtr& Game::getBoard()
{
    return mBoard;
}

const Player* Game::getPlayer1()
{
    return mPlayer1.get();
}

const Player* Game::getPlayer2()
{
    return mPlayer2.get();
}

const Player* Game::getActivePlayer()
{
    return mActivePlayer.get();
}


