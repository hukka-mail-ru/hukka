#ifndef SYSTEM_H_
#define SYSTEM_H_



bool isAppAlreadyRunning();
long getTime(); // get time in milliseconds  
void millisleep(unsigned milliseconds);

#ifdef WIN_BUILD
    #include "stdafx.h"
    #include <cstdlib> 
    time_t time( time_t *inTT );
#else
    #include <string> 
    std::string getCurDir();
#endif


    
    
#endif /*SYSTEM_H_*/
