#ifndef SYSTEM_H_
#define SYSTEM_H_

#include <string> 
std::string EDR_GetCurDir();

bool EDR_IsAppAlreadyRunning();
long EDR_GetTime(); // get time in milliseconds  
void EDR_Millisleep(unsigned milliseconds);

// EDR_GetRandomNumber ? based on Random() - Windows Mobile.

#ifdef _WIN32
    #include <windows.h>
    #include <cstdlib> 
    time_t time( time_t *inTT );
    
    #define M_PI 3.141592
#endif


    
    
#endif /*SYSTEM_H_*/
