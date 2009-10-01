#ifndef MUTEX_H
#define MUTEX_H

/**
@author Aleksey P. Karelin
*/


#include <pthread.h>

class CMutex
{
public:
    CMutex();

    ~CMutex();
    
    bool lock(); 	
    bool tryLock();	
    bool unlock();	

private:
    pthread_mutex_t m_Mutex;
};

#endif
