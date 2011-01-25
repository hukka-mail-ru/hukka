//#include "stdafx.h"
#include <iostream>
#include "mythread.h"

#ifndef WIN32


typedef void* (*TFunc)( void* );

void* ThreadFunc( void* _pParam )
{
    ( static_cast<CMyThread*>(_pParam) )->Run();
}



CMyThread::CMyThread()
{
}

CMyThread::~CMyThread()
{
}


void CMyThread::Continue()
{
    pthread_create( &m_hThread, NULL, ThreadFunc, this );
}

void CMyThread::killThread()
{
	pthread_cancel(m_hThread);
}

void CMyThread::stopThread(bool _status)
{
	if ( _status )
		pthread_mutex_lock(&m_Mutex);
	else
		pthread_mutex_unlock(&m_Mutex);
}


#endif
