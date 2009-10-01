// 
// File:   mythread.h
// Author: leha
//
// Created on 18 јпрель 2008 г., 14:12
//

#ifndef _MYTHREAD_H
#define	_MYTHREAD_H

#include <pthread.h>

class CMyThread 
{
public:
    CMyThread();
    virtual ~CMyThread();
    
    void    Continue();
    void    killThread();
    void    stopThread( bool _bStatus );
    
    virtual int Run() = 0;

private:
    
    pthread_t       m_hThread;
    pthread_mutex_t m_Mutex;
    
};


#endif	/* _MYTHREAD_H */

