#ifndef SYSTEM_H_
#define SYSTEM_H_

#ifdef LINUX_BUILD
#include <sys/types.h>

    pid_t readPID(const char* pidfile);
    void writePID(const char* pidfile);
    
    bool isRunning(pid_t pid);
#endif

#ifdef WIN_BUILD
    time_t time( time_t *inTT )
#endif
    
#endif /*SYSTEM_H_*/
