#include <stdio.h>
#include <iostream>
using namespace std;

#include "Game.h"
#include "UI.h"
#include "Macros.h"
#include "System.h"

#ifdef LINUX_BUILD
int main(int argc, char *argv[])
#endif 

#ifdef WIN_BUILD
int _tmain(int argc, _TCHAR* argv[])
#endif
{
    TRY_BEGINS;

    if(isAppAlreadyRunning())
        return -1;
    
    GamePtr game = GamePtr(new Game());    
    game->startup();
    
    UIPtr ui = UIPtr(new UI(game)); 
    ui->startup();
    
    ui->handleEvents();
   
    TRY_CATCH;
    
    return 0;
}
