#include "System.h"

using namespace std;

#ifdef LINUX_BUILD

#include <sys/types.h>
#include <signal.h>
#include <unistd.h>
#include <stdlib.h>
#include <string>
#include <fstream>


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

bool isRunning(pid_t pid)
{
    return (kill(pid,0)==0) ? true : false;  // return 0 if the process exists, else -1
}



#endif
