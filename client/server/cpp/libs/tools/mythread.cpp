#include "mythread.h"

void* ThreadFunc( void* _pParam )
{
	CMyThread* pMyThread = static_cast<CMyThread*>( _pParam );

	if( pMyThread )
		int nRes = pMyThread->Run();

	return pMyThread;
}

CMyThread::CMyThread()
	:m_hThread( NULL )
{
	
}

CMyThread::~CMyThread()
{

}

pthread_t CMyThread::GetID() const
{
	return m_hThread;
}

int CMyThread::StartLoop()
{
	return Continue();
}

void CMyThread::StopLoop()
{
	pthread_cancel( m_hThread );
	pthread_join( m_hThread, NULL );
}

int CMyThread::Continue()
{
//	if( m_hThread )
//		return 0;

	return pthread_create( &m_hThread, 0, ThreadFunc, static_cast<CMyThread*>( this ) );
}
