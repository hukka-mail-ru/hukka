#ifndef UI_H_
#define UI_H_

#include "../include/Macros.h"
#include "Game.h"

CLASSPTR(UI);

class UI
{
public:
    
    UI(const GamePtr& game): mGame(game), mQuit(false)
    {}
    
    bool startup();
    
    void waitForEvents();
    
private:
    
    bool stop(bool res);
    bool resizeWindow(unsigned width, unsigned height);
    bool initGL();
    bool drawBoard();
    
    
    GamePtr mGame; 
    bool mQuit;
    
};


#endif /*UI_H_*/
