#include <iostream>
using namespace std;

#include "Game.h"
#include "UI.h"
#include "Macros.h"



int main()
{
    TRY_BEGINS;
    
    GamePtr game = GamePtr(new Game());    
    game->startup();
    
    UIPtr ui = UIPtr(new UI(game)); 
    ui->startup();
    
    ui->handleEvents();
   
    exit(0);
    TRY_CATCH;
   
    return 0;
}
