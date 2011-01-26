#include "socketmanager.h"

#include <iostream>

using namespace std;

void SocketManager::AddInMsg( MySocket* _pMySocket )
{
	pthread_mutex_lock( &m_mutDeqSocket );
	m_queSocket.push( _pMySocket );
	pthread_mutex_unlock( &m_mutDeqSocket );

	sem_post( &m_semDeqSocket );
}

void SocketManager::AddOutMsg( MySocket* _pSocket )
{
    cout << "SocketManager::AddOutMsg AddHandle to socket " << _pSocket->GetSocket() << endl;
	m_pSelector->AddHandle( _pSocket->GetSocket(), EPOLLOUT | EPOLLONESHOT, static_cast<ICallBack*>( _pSocket ) );
}

void SocketManager::OnClose( MySocket* _pSocket )
{
    cout << "SocketManager::OnClose AddHandle" << endl;
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
