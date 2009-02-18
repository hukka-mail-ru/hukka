#include <iostream>
using namespace std;

#include "Game.h"
#include "UI.h"
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
    
    GamePtr game = GamePtr(new Game());    
    game->startup();
    
    UIPtr ui = UIPtr(new UI(game)); 
    ui->startup();
    
    ui->waitForEvents();
   
    exit(0);
    TRY_CATCH;
   
    return 0;
}
