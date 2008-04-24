#ifndef __PING_HPP__
#define __PING_HPP__


enum PingResult
{
    SUCCESS,
    ERROR,
    SILENCE
};    
    

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

