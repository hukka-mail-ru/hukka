#ifndef SYSTEM_H_
#define SYSTEM_H_

bool isAppAlreadyRunning();
long getTime(); // get absolute time in microseconds 


#ifdef WIN_BUILD
    time_t time( time_t *inTT )
#endif
    
#endif /*SYSTEM_H_*/
