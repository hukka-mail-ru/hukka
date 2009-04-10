#ifndef SYSTEM_H_
#define SYSTEM_H_



bool EDR_IsAppAlreadyRunning();
long EDR_GetTime(); // get time in milliseconds  
void EDR_Millisleep(unsigned milliseconds);

#ifdef _WIN32
    #include "stdafx.h"
    #include <cstdlib> 
    time_t time( time_t *inTT );
    
    #define M_PI 3.141592
    
    #include <string>
    std::wstring EDR_GetCurDir();
    
#else
    #include <string> 
    std::string EDR_GetCurDir();
#endif


    
    
#endif /*SYSTEM_H_*/
