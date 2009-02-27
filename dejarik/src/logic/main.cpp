#include <stdio.h>
#include <iostream>
using namespace std;

#include "Game.h"
#include "UI.h"
#include "Macros.h"

// PLATFORM-DEPENDENT
// ----------------------------------------------------------------------
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/file.h>

#define LOCKED_FILE "locked_file"
int fd = 0;

bool lockFile()
{
    fd = open(LOCKED_FILE, O_CREAT);
    if(fd == -1) 
        return false;
    
    // Place an exclusive lock
    if(flock(fd, LOCK_EX) == -1) 
        return false;
    
    return true;
}


bool unlockFile()
{
    // try to unlock; erase file if unlocking failed
    if(flock(fd, LOCK_UN) == -1) 
        return false;

    if(unlink(LOCKED_FILE) == -1)
        return false;
    
    return true;
}
// ----------------------------------------------------------------------


int main()
{
    TRY_BEGINS;

    if(!lockFile())
        return -1;
    
    GamePtr game = GamePtr(new Game());    
    game->startup();
    
    UIPtr ui = UIPtr(new UI(game)); 
    ui->startup();
    
    ui->handleEvents();
   
    TRY_CATCH;

    return unlockFile();
}
