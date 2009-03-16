#include <stdio.h>
#include <iostream>
using namespace std;

#include "Game.h"
#include "UI.h"
#include "Macros.h"

#ifdef LINUX_BUILD
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
        
        if(flock(fd, LOCK_EX | LOCK_NB) == -1) 
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
#endif

int main(int argc, char *argv[])
{
    TRY_BEGINS;

#ifdef LINUX_BUILD
    if(!lockFile())
        return -1;
#endif
    
    GamePtr game = GamePtr(new Game());    
    game->startup();
    
    UIPtr ui = UIPtr(new UI(game)); 
    ui->startup();
    
    ui->handleEvents();
   
    TRY_CATCH;

#ifdef LINUX_BUILD
    return unlockFile();
#endif
    
    return 0;
}
