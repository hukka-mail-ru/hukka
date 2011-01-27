#include "socketmanager.h"

#include <iostream>

using namespace std;

void SocketManager::AddInMsg( MySocket* _pSocket )
{
#ifdef LOW_LEVEL_DEBUG
    cout << "SOCKET " << _pSocket->GetSocket() << " SocketManager::AddInMsg" << endl;
#endif

	pthread_mutex_lock( &m_mutDeqSocket );
	m_queSocket.push( _pSocket );
	pthread_mutex_unlock( &m_mutDeqSocket );

	sem_post( &m_semDeqSocket );
}

void SocketManager::AddOutMsg( MySocket* _pSocket )
{
#ifdef LOW_LEVEL_DEBUG
    cout << "SOCKET " << _pSocket->GetSocket() << " SocketManager::AddOutMsg" << endl;
#endif

	m_pSelector->AddHandle( _pSocket->GetSocket(), EPOLLOUT | EPOLLONESHOT, static_cast<ICallBack*>( _pSocket ) );
}

void SocketManager::RemoveSocket( MySocket* _pSocket )
{
#ifdef LOW_LEVEL_DEBUG
    cout << "SOCKET " << _pSocket->GetSocket() << " SocketManager::RemoveSocket" << endl;
#endif
	m_pSelector->RemoveHandle( _pSocket->GetSocket(), EPOLLIN | EPOLLOUT, static_cast<ICallBack*>( _pSocket ) );
}

SocketManager::SocketManager( int _nCountThread )
{
    pthread_mutex_init(&this->m_mutDeqSocket, NULL);
	m_pSelector = Selector::Instance();

	sem_init( &m_semDeqSocket, 0 , 0 );

	for( int i = 0 ; i < _nCountThread ; ++i )
		StartLoop();
}

SocketManager::~SocketManager()
{
	Selector::FreeInst();
}

int SocketManager::Run()
{
	for(;;)
	{
		sem_wait( &m_semDeqSocket );

		MySocket* pSocket = 0;

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
