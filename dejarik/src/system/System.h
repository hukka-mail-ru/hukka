#ifndef SYSTEM_H_
#define SYSTEM_H_



bool EDR_IsAppAlreadyRunning();
long EDR_GetTime(); // get time in milliseconds  
void EDR_Millisleep(unsigned milliseconds);

#ifdef WIN_BUILD
    #include "stdafx.h"
    #include <cstdlib> 
    time_t time( time_t *inTT );
#else
    #include <string> 
    std::string EDR_GetCurDir();
#endif


    
    
#endif /*SYSTEM_H_*/
