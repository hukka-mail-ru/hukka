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
    BattleResult onCellClick(const CellPtr& cell);

    const BoardPtr& getBoard();
    
    // needed by UI:
    const Player* getPlayer1();
    const Player* getPlayer2();
    const Player* getActivePlayer();
    
private:
        
    void passTurn(); // pass turn to the next player
    
    bool checkVictory(PlayerPtr& vinner);
    
    BoardPtr mBoard;
    
    PlayerPtr mPlayer1;
    PlayerPtr mPlayer2;
    PlayerPtr mActivePlayer;
    
};



#endif /*GAME_H_*/
