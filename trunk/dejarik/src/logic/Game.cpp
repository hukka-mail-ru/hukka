#include <vector>
#include <sstream>

#include "Game.h"

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
    mBoard->definePossibleClicks(mActivePlayer, false);
    
    TRY_RETHROW;
}


void Game::passTurn()
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
    
    if(mActivePlayer->getLeftMoves() == 0)
    {
        mActivePlayer->setLeftMoves(2);
    }
    
    TRY_RETHROW;
}


bool Game::isOver()
{
    TRY_BEGINS;
    
    PlayerPtr vinner;
    bool res = checkVictory(vinner);
    
    // TODO show vinner
    
    return res;
    
    TRY_RETHROW;
}

    
void Game::onCellClick(const CellPtr& cell)
{
    TRY_BEGINS;
    
    mBoard->deselectAll();
    
    if(!mBoard->isClickValid(cell))
    {
        return;
    }
    
    if(!cell->piece) // empty cell
    {
        mActivePlayer->movePiece(cell);
        mActivePlayer->decrementLeftMoves();
        mBoard->definePossibleClicks(mActivePlayer, false);
    }
    else // clicked on a piece
    {
        if(cell->piece->player == mActivePlayer) // mine
        {
            mActivePlayer->setActivePiece(cell->piece);
            mBoard->definePossibleClicks(mActivePlayer, false);
        }
        else // enemy's
        {
            BattleResult res = mActivePlayer->attackEnimy(cell->piece);
            
            if(res == RES_KILL)
            {
                mBoard->killPiece(const_cast<PiecePtr&>(cell->piece));
                mActivePlayer->decrementLeftMoves();
                mBoard->definePossibleClicks(mActivePlayer, false);
            }
            else if(res == RES_COUNTER_KILL)
            {
                mBoard->killPiece(const_cast<PiecePtr&>(mActivePlayer->getActivePiece()));
                mActivePlayer->decrementLeftMoves();
                mBoard->definePossibleClicks(mActivePlayer, false);
            }
            else if(res == RES_PUSH)
            {
                mBoard->definePossibleClicks(mActivePlayer, true ); 
                mActivePlayer->incrementLeftMoves();
            }
            else if(res == RES_COUNTER_PUSH)
            {
                mActivePlayer->decrementLeftMoves();
                
                PlayerPtr enemy = (mActivePlayer == mPlayer1) ? mPlayer2 : mPlayer1; // next player
                mBoard->definePossibleClicks(enemy, true );
                enemy->setLeftMoves(1);
                passTurn();
                return;
            }
            
        }
        
        
    }
    
    if(mActivePlayer->getLeftMoves() == 0)
    {
        passTurn();
    }
    
    
   /* 
    mBoard->deselectAll();

    static unsigned move = 0; // a player has 2 moves 
    static TurnStage stage = TURN_SELECTION; // each move has two phases: START (selection), FINISH (action)   
    static BattleResult battleResult = RES_NO_BATTLE;
    
    if(cell->piece)
    {
        cout << "Player " << (long)mActivePlayer.get() << 
                "; Piece " << cell->piece->name << 
                "; Move: " << cell->piece->moveRating << 
                "; Stage:" << stage << endl;
    }
  
    
    // if clicked on an ally piece, then TURN_SELECTION
    if(battleResult != RES_PUSH && battleResult != RES_COUNTER_PUSH &&
       cell->piece && cell->piece->player == mActivePlayer)
    {
        stage = TURN_SELECTION;
    }
    
    if(stage == TURN_SELECTION)
    {
        if(mActivePlayer->makeTurn(cell, stage, battleResult))
        {
            stage = TURN_ACTION;
            
            mBoard->selectClickedCell(cell);
            return true;
        }
    }
    else if(stage == TURN_ACTION)
    {
        if(battleResult == RES_NO_BATTLE)
        {
            if(mActivePlayer->makeTurn(cell, stage, battleResult))
            {
                if(battleResult == RES_NO_BATTLE ||
                   battleResult == RES_KILL ||
                   battleResult == RES_COUNTER_KILL)
                {
                    stage = TURN_SELECTION;
                    move++;
                }
                
                if(move == 2)
                {
                    mActivePlayer = (mActivePlayer == mPlayer1) ? mPlayer2 : mPlayer1; // next player
                    mBoard->deselectAll();
                    mBoard->selectAll(mActivePlayer);
                    move = 0;
                }
                
              //  mBoard->selectClickedCell(cell);
                return true;
            }
        }
        else if(battleResult == RES_PUSH || battleResult == RES_COUNTER_PUSH)
        {
            if(mActivePlayer->makePush(cell)) // without verification of piece's owner
            {
                stage = TURN_SELECTION;
                move++;
                
                if(move == 2)
                {
                    mActivePlayer = (mActivePlayer == mPlayer1) ? mPlayer2 : mPlayer1; // next player
                    mBoard->selectAll(mActivePlayer);
                    move = 0;
                }
                
             //   mBoard->selectClickedCell(cell);
                return true;
            }
        }
    }
    
    return false;
*/    
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

