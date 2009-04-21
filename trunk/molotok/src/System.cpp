#include "System.h"

using namespace std;



#include <sys/types.h>
#include <sys/time.h>
#include <signal.h>
#include <unistd.h>
#include <stdlib.h>
#include <string>
#include <fstream>
#include <stdexcept>
#include <time.h>

void writePID(const char* pidfile)
{
    ofstream out(pidfile);
    out << getpid();
}

pid_t readPID(const char* pidfile)
{
    ifstream in(pidfile);
    
    string str;
    getline(in, str);
    
    return atoi(str.c_str());
}

bool EDR_IsAppAlreadyRunning()
{
    pid_t pid = readPID("pidfile.txt");
    
    if(pid != 0 && kill(pid,0) == 0) // kill returns 0 if the process exists, else -1
        return true;
    
    writePID("pidfile.txt");
    
    return false;
}


// get time in milliseconds 
long EDR_GetTime()
{
    timeval tv;
    if(gettimeofday(&tv, NULL) == -1)
        throw runtime_error("gettimeofday failed");
    
    return tv.tv_sec*1000 + tv.tv_usec/1000;
}

string EDR_GetCurDir() 
{
    return "";
}

void EDR_Millisleep(unsigned milliseconds)
{
    timespec delay;
    delay.tv_sec = 0;
    delay.tv_nsec = milliseconds * 1000000;

    nanosleep(&delay, NULL);
}


