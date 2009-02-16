#ifndef GAME_H_
#define GAME_H_

#include "Macros.h"
#include "Board.h"
#include "Player.h"

CLASSPTR(Game);


class Game
{
public:
    
    void start();

    void finish();
    
private:
    
    void askNextPlayer();
    
    bool checkVictory(PlayerPtr& vinner);
    
    BoardPtr mBoard;
    
    PlayerPtr mPlayer1;
    PlayerPtr mPlayer2;
    
};



#endif /*GAME_H_*/
