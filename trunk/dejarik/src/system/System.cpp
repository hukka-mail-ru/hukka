#include "System.h"

using namespace std;

#ifdef LINUX_BUILD

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

#endif



#ifdef _WIN32

bool EDR_IsAppAlreadyRunning()
{
    // TODO not implemented yet
    return false;
}

long EDR_GetTime()
{
    return GetTickCount();
}

time_t time( time_t *inTT )
{
    SYSTEMTIME sysTimeStruct;
    FILETIME fTime;
    ULARGE_INTEGER int64time;
    time_t locTT = 0;

    if ( inTT == NULL ) 
    {
        inTT = &locTT;
    }

    GetSystemTime( &sysTimeStruct );
    if ( SystemTimeToFileTime( &sysTimeStruct, &fTime ) ) 
    {
        memcpy( &int64time, &fTime, sizeof( FILETIME ) );
        /* Subtract the value for 1970-01-01 00:00 (UTC) */
        int64time.QuadPart -= 0x19db1ded53e8000;
        /* Convert to seconds. */
        int64time.QuadPart /= 10000000;
        *inTT = (time_t)int64time.QuadPart;
    }
    return *inTT;
} 

void EDR_Millisleep(unsigned milliseconds)
{
    Sleep(milliseconds);
}

#define MAXPATHLEN 1024
string EDR_GetCurDir() // TODO change to string
{
    WCHAR buf[MAXPATHLEN];
    GetModuleFileName(0, buf, MAXPATHLEN);
    wstring dir(buf);
    
    size_t pos = dir.rfind(L"\\");
    dir = dir.substr(0, pos);
    
    string str;
    str.assign(dir.begin(), dir.end());
    
    str += "\\";
    
    return str;
}
#endif

