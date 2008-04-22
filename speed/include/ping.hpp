#ifndef __PING_HPP__
#define __PING_HPP__

class Pinger
{
public:

    int ping(const char* address);

    long long getElapsed() { return mElapsed; }
    long long getBytes() { return mBytes; }

private:

    long long mElapsed;
    long long mBytes;
    
};

#endif

