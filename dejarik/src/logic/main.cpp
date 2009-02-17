#include <iostream>
using namespace std;

#include "Game.h"
#include "Macros.h"

/*
void onMouseClick(unsigned x, unsigned y) 
{
    unsigned cellC = 0; // cell coordinate
    unsigned cellX = 0; // cell coordinate
    if(game.isCell(x, y, cellC, cellX))
    {
        game.onCellClick(cellC, cellX);
    }
}*/


int main()
{
    TRY_BEGINS;
    
    Game game;
    
    game.start();
    
    while(!game.isOver())
    {
        sleep(1);
    }
    
    TRY_CATCH;
   
    return 0;
}
