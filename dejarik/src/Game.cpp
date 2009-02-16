#include <vector>

#include "Game.h"

using namespace std;

void Game::start()
{
    mBoard = BoardPtr(new Board());
    
    // TODO ask for player name
    
    mPlayer1 = PlayerPtr(new Player("Player1", mBoard));
    mPlayer1 = PlayerPtr(new Player("Player2", mBoard));

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
    PlayerPtr player = mPlayer1;
    for(unsigned i=0; i<pieces_num; i++)
    {
        unsigned rnd = (rand()%pieces.size()); // random 0.. pieces.size() - 1
        
        mBoard->placePiece(pieces[rnd], cells[i]->c, cells[i]->x);
        mBoard->distribute(pieces[rnd], player);
        
        player = (player == mPlayer1) ? mPlayer2 : mPlayer1; // next player
        pieces.erase(remove(pieces.begin(), pieces.end(), pieces[rnd]), pieces.end()); 
    }
    
}

void Game::finish()
{
    
}
    
void Game::askNextPlayer()
{
    
}
    
bool Game::checkVictory(PlayerPtr& vinner)
{
    return false;
}

