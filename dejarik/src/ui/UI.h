#ifndef UI_H_
#define UI_H_

#include "Macros.h"
#include "Game.h"

CLASSPTR(UI);

class UI
{
public:
    
    UI(const GamePtr& game): mGame(game)
    {}
    
    bool startup();
    
    void waitForEvents();
    
private:
    
    bool stop(bool res);
    bool resizeWindow(unsigned width, unsigned height);
    bool initGL();
    bool drawBoard();
    
    
    GamePtr mGame; 
    
};


#endif /*UI_H_*/
