#include "socketmanager.h"

//#include <iostream>

void CSocketManager::AddInMsg( CMySocket* _pMySocket )
{
	pthread_mutex_lock( &m_mutDeqSocket );
	m_queSocket.push( _pMySocket );
	pthread_mutex_unlock( &m_mutDeqSocket );

	sem_post( &m_semDeqSocket );
}

void CSocketManager::AddOutMsg( CMySocket* _pSocket )
{
	m_pSelector->AddHandle( _pSocket->GetSocket(), EVFILT_WRITE, EV_ONESHOT, static_cast<ICallBack*>( _pSocket ), true );
}

void CSocketManager::OnClose( CMySocket* _pSocket )
{
	m_pSelector->AddHandle( _pSocket->GetSocket(), EVFILT_READ, EV_DELETE, static_cast<ICallBack*>( _pSocket ) );
}

CSocketManager::CSocketManager( int _nCountThread )
	:m_mutDeqSocket( PTHREAD_MUTEX_INITIALIZER )
{
	m_pSelector = CSelector::Instance();

	sem_init( &m_semDeqSocket, 0 , 0 );

	for( int i = 0 ; i < _nCountThread ; ++i )
		StartLoop();
}

CSocketManager::~CSocketManager()
{
	CSelector::FreeInst();
}

int CSocketManager::Run()
{
	for(;;)
	{
		sem_wait( &m_semDeqSocket );

		CMySocket* pSocket = 0;

		pthread_mutex_lock( &m_mutDeqSocket );

		if( !m_queSocket.empty() )
		{
			pSocket = m_queSocket.front();
			m_queSocket.pop();
		}

		pthread_mutex_unlock( &m_mutDeqSocket );

		if( pSocket == 0 )
			continue;

		DoAllMsg( pSocket );
	}
}
