#include <vector>
#include <sstream>

#include "Game.h"

using namespace std;

void Game::start()
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
    
    // randomly divide the pieces between the players, 
    // and place them on opposites sides of the board
    srand((unsigned)time(0)); 
    const unsigned pieces_num = pieces.size();
    PlayerPtr player = mPlayer1;
    
    for(unsigned i=0; i<pieces_num; i++)
    {
        unsigned rnd = (rand()%pieces.size()); // random 0.. pieces.size() - 1
        
        mBoard->placePiece(pieces[rnd], cells[i]);
        mBoard->distribute(pieces[rnd], player);
        
        player = (player == mPlayer1) ? mPlayer2 : mPlayer1; // next player
        pieces.erase(remove(pieces.begin(), pieces.end(), pieces[rnd]), pieces.end()); 
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

    
bool Game::onCellClick(unsigned c, unsigned x)
{
    TRY_BEGINS;
    
    static PlayerPtr player = mPlayer1; // Player1 makes the first move
    static TurnStage stage = TURN_START;    
    static BattleResult battleResult = RES_NO_BATTLE;
    
    if(stage == TURN_START)
    {
        if(player->makeTurn(c, x, stage, battleResult))
        {
            stage = TURN_FINISH;
            return true;
        }
    }
    else if(stage == TURN_FINISH)
    {
        if(battleResult == RES_NO_BATTLE)
        {
            if(player->makeTurn(c, x, stage, battleResult))
            {
                if(battleResult == RES_NO_BATTLE ||
                   battleResult == RES_KILL ||
                   battleResult == RES_COUNTER_KILL)
                {
                    stage = TURN_START;
                    player = (player == mPlayer1) ? mPlayer2 : mPlayer1; // next player
                    return true;
                }
            }
        }
        else if(battleResult == RES_PUSH || battleResult == RES_COUNTER_PUSH)
        {
            if(player->makePush(c, x))
            {
                stage = TURN_START;
                player = (player == mPlayer1) ? mPlayer2 : mPlayer1; // next player
                return true;
            }
        }
    }
    
    return false;
    
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

