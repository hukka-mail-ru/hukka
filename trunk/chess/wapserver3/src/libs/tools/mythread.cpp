#include "mythread.h"

void* ThreadFunc( void* _pParam )
{
	MyThread* pMyThread = static_cast<MyThread*>( _pParam );

	if( pMyThread )
		int nRes = pMyThread->Run();

	return pMyThread;
}

MyThread::MyThread()
	:m_hThread( NULL )
{
	
}

MyThread::~MyThread()
{

}

pthread_t MyThread::GetID() const
{
	return m_hThread;
}

int MyThread::StartLoop()
{
	return Continue();
}

void MyThread::StopLoop()
{
	pthread_cancel( m_hThread );
	pthread_join( m_hThread, NULL );
}

int MyThread::Continue()
{
//	if( m_hThread )
//		return 0;

	return pthread_create( &m_hThread, 0, ThreadFunc, static_cast<MyThread*>( this ) );
}
