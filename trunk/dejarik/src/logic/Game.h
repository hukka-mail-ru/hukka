#ifndef GAME_H_
#define GAME_H_

#include "../common/Macros.h"
#include "Board.h"
#include "Player.h"
#include "Cell.h"

CLASSPTR(Game);


class Game
{
    friend class TestGame;
    
public:
    
    void startup();
    
    bool isOver();
    
    /*
     * Reurns true if the click is allowed by the game rules 
     */
    bool onCellClick(const CellPtr& cell);

    const BoardPtr& getBoard();
    
private:
        
    bool checkVictory(PlayerPtr& vinner);
    
    BoardPtr mBoard;
    
    PlayerPtr mPlayer1;
    PlayerPtr mPlayer2;
    PlayerPtr mActivePlayer;
    
};



#endif /*GAME_H_*/
