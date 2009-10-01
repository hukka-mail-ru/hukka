#include <queue>
#include <errno.h>
#include <signal.h>
#include "../header/interface.h"
#include "selector.h"

//#include <iostream>
#include <syslog.h>

CSelector* CSelector::m_pSelf = 0;
int CSelector::m_nRefCount = 0;

CSelector* CSelector::Instance()
{
	if( m_pSelf == 0 )
		m_pSelf = new CSelector;
	
	++m_nRefCount;
	
	return m_pSelf;
}

void CSelector::FreeInst()
{
	--m_nRefCount;
	
	if( m_nRefCount > 0 )
		return;
	
	KillObject();
}

void CSelector::KillObject()
{
	if( m_pSelf != 0 )
		delete m_pSelf;
	
	m_pSelf = 0;
	m_nRefCount = 0;
}

void CSelector::AddHandle( int _Socket, short _Events, u_short _Flags, ICallBack* _pCallBack, bool _isSendSigna )
{
	struct kevent kev;
	EV_SET( &kev, _Socket, _Events, _Flags, 0, 0, _pCallBack );

	pthread_mutex_lock( &m_mutAddE );
	m_vecEvChange.push_back( kev );
	pthread_mutex_unlock( &m_mutAddE );

//	if( _isSendSigna )
//		kill( getpid(), SIGUSR1 );
}

int CSelector::StartLoop()
{
	if( pthread_mutex_trylock( &m_mutLoop ) == EBUSY )
		return 0;
	
	int nRes = MainLoop();
	
	pthread_mutex_unlock( &m_mutLoop );
	
	return nRes;
}

void CSelector::StopLoop()
{
	m_isContinue = false;

	kill( getpid(), SIGUSR1 );
}

CSelector::CSelector()
	:m_mutLoop( PTHREAD_MUTEX_INITIALIZER ),
	 m_mutAddE( PTHREAD_MUTEX_INITIALIZER ),
	 m_isContinue( false )
{
	signal( SIGUSR1, SIG_IGN );
	struct kevent kev;
	EV_SET( &kev, SIGUSR1, EVFILT_SIGNAL, EV_ADD, 0, 0, 0 );

	pthread_mutex_lock( &m_mutAddE );
	m_vecEvChange.push_back( kev );
	pthread_mutex_unlock( &m_mutAddE );
}

CSelector::~CSelector()
{

}

int CSelector::MainLoop()
{
	int nKq, n;
	ICallBack* pCallBack;
	TVecKevent vecEvChange, vecEvList;
	vecEvList.resize( 1024 );

	timespec tmStep;
	tmStep.tv_sec = 0;
	tmStep.tv_nsec = 500000000;

	if( ( nKq = kqueue() ) < 0 )
		return errno;

	m_isContinue = true;

	while( m_isContinue )
	{
		pthread_mutex_lock( &m_mutAddE );
		vecEvChange.assign( m_vecEvChange.begin(), m_vecEvChange.end() );
		m_vecEvChange.clear();
		pthread_mutex_unlock( &m_mutAddE );
		
		if( ( n = kevent( nKq, &vecEvChange[0], vecEvChange.size(), &vecEvList[0], vecEvList.size(), &tmStep ) ) == -1 )
			return errno;

		for( struct kevent* pKevent = &vecEvList[0] ; pKevent < &vecEvList[n] ; ++pKevent )
		{
			if( pKevent->filter == EVFILT_SIGNAL )
			{
				//syslog( LOG_INFO | LOG_LOCAL0, "MainLoop()-Signal" );
				//std::cout << "CSocketManager::MainLoop()-Signal" << std::endl;
				continue;
			}
			if( ( pCallBack = static_cast<ICallBack*>(pKevent->udata) ) == 0 )
			{
				//syslog( LOG_INFO | LOG_LOCAL0, "MainLoop()-NULL-Err!!!" );
				//std::cout << "CSocketManager::MainLoop()-NULL-Err!!!" << std::endl;
				continue;
			}
			if( pKevent->filter == EVFILT_READ )
			{
				//syslog( LOG_INFO | LOG_LOCAL0, "MainLoop()-EVFILT_READ" );
				//std::cout << "CSocketManager::MainLoop()-EVFILT_READ "<< pKevent->ident << std::endl;
				pCallBack->DoRead();
			}
			else if( pKevent->filter == EVFILT_WRITE )
			{
				//syslog( LOG_INFO | LOG_LOCAL0, "MainLoop()-EVFILT_WRITE" );
				//std::cout << "CSocketManager::MainLoop()-EVFILT_WRITE "<< pKevent->ident  << std::endl;
				pCallBack->DoWrite();
			}
			else
			{
				//syslog( LOG_INFO | LOG_LOCAL0, "MainLoop()-Unknown" );
				//std::cout << "CSocketManager::MainLoop()-Unknoun" << std::endl;
			}
		}
	}
}
