#include <stdio.h>
#include <iostream>
using namespace std;

#include "Game.h"
#include "UI.h"
#include "Macros.h"
#include "System.h"


int main(int argc, char *argv[])
{
    TRY_BEGINS;

#ifdef LINUX_BUILD
    pid_t pid = readPID("pidfile.txt");
    
    if(pid != 0 && isRunning(pid)) 
        return -1;
    
    writePID("pidfile.txt");
#endif
    
    GamePtr game = GamePtr(new Game());    
    game->startup();
    
    UIPtr ui = UIPtr(new UI(game)); 
    ui->startup();
    
    ui->handleEvents();
   
    TRY_CATCH;
    
    return 0;
}
