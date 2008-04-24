#ifndef __PING_HPP__
#define __PING_HPP__



enum PingResult
{
    SUCCESS,
    ERROR,
    SILENCE
};    
    

// --------------------------------------------------------------------------------
//
// This class operates in the 3-rd (Network) level of OSI
//
// --------------------------------------------------------------------------------  

class Pinger
{
public:

    PingResult ping(const char* address, unsigned time_limit = 0);

    long long getElapsedTime() { return mElapsed; }
    long long getBytes() { return mBytes; }

private:

    long long mElapsed;
    long long mBytes;
    
};

#endif

