#include <queue>
#include <errno.h>
#include <signal.h>
#include "../header/interface.h"
#include "selector.h"

#include <iostream>
#include <assert.h>
#include <syslog.h>
#include <stdlib.h>
#include <errno.h>

using namespace std;

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

void CSelector::AddHandle( int _Socket, unsigned _Event, unsigned _Flags, ICallBack* _pCallBack)
{

    struct epoll_event ev;

    ev.events = _Flags;
    ev.data.ptr = _pCallBack;

    int res = epoll_ctl(m_epfd, _Event, _Socket, &ev);

 /*   cout << "epoll_ctl ADD (" << (unsigned)_pCallBack << "): Socket: " << _Socket << ", Flags : "<< _Flags << endl;
    if(res != 0)
    {
        cout << "ERROR " << errno << ": " << strerror(errno) << endl;
    }
*/
    if(_Event == EPOLL_CTL_ADD && errno == EEXIST)
    {
        int res = epoll_ctl(m_epfd, EPOLL_CTL_MOD, _Socket, &ev);

   //     cout << "epoll_ctl MOD: Socket: " << _Socket << ", Flags : "<< _Flags << endl;
        if(res != 0)
        {
    //        cout << "ERROR " << errno << ": " << strerror(errno) << endl;
            exit(1);
        }
    }

    if(_Flags & EPOLLONESHOT)
    {
        m_WritingSocket = _Socket;
    }

    /*
	struct kevent kev;
	EV_SET( &kev, _Socket, _Events, _Flags, 0, 0, _pCallBack );

	pthread_mutex_lock( &m_mutAddE );
	m_vecEvChange.push_back( kev );
	pthread_mutex_unlock( &m_mutAddE );
	*/
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

#define MAX_LISTEN_PORT_NUM 1024
#define MAX_EPOLL_EVENTS_PER_RUN 1024
#define EPOLL_RUN_TIMEOUT 1000 //milliseconds

CSelector::CSelector():
	 m_isContinue( false )
{

    pthread_mutex_init(&this->m_mutLoop, NULL);
    pthread_mutex_init(&this->m_mutAddE, NULL);
    pthread_mutex_init(&this->m_mutFd, NULL);

	signal( SIGUSR1, SIG_IGN );

	pthread_mutex_lock( &m_mutFd );
	m_epfd = epoll_create(MAX_LISTEN_PORT_NUM);
	assert(m_epfd >= 0);
	pthread_mutex_unlock( &m_mutFd );

/*
	struct kevent kev;
	EV_SET( &kev, SIGUSR1, EVFILT_SIGNAL, EV_ADD, 0, 0, 0 );

	pthread_mutex_lock( &m_mutAddE );
	m_vecEvChange.push_back( kev );
	pthread_mutex_unlock( &m_mutAddE );
	*/
}

CSelector::~CSelector()
{

}

int CSelector::MainLoop()
{
	m_isContinue = true;

	while( m_isContinue )
	{
	    epoll_event events[MAX_EPOLL_EVENTS_PER_RUN];

        int nfds = epoll_wait(m_epfd, events, MAX_EPOLL_EVENTS_PER_RUN, EPOLL_RUN_TIMEOUT);

        if(nfds > 0)
        {
      //      cout << "wait: got " << nfds << " event(s). ERROR " << errno << ": " << strerror(errno) << endl;
        }

        assert(nfds >= 0);

        for(int i = 0; i < nfds; i++)
        {
            if(events[i].events & EPOLLIN)
            {
                //cout << "DoRead <" << (unsigned)events[i].data.ptr << ">" << endl;
                static_cast<ICallBack*>(events[i].data.ptr)->DoRead();
            }
            else if(events[i].events & EPOLLOUT)
            {
                cout << "DoWrite {" << (unsigned)events[i].data.ptr << "}" << endl;
                static_cast<ICallBack*>(events[i].data.ptr)->DoWrite();

               // cout << "DoWrite ends" << endl;
                // rearm
               // if(events[i].events & EPOLLONESHOT)
              //  {
                 //   cout << "Rearm" << endl;
                    AddHandle(m_WritingSocket, EPOLL_CTL_ADD, EPOLLIN, static_cast<ICallBack*>(events[i].data.ptr));
             //   }
            }
            else if(events[i].events & EPOLLPRI)
            {
                cout << "EPOLLPRI" << endl;
            }
            else if(events[i].events & EPOLLERR)
            {
                cout << "EPOLLERR" << endl;
            }
            else if(events[i].events & EPOLLHUP)
            {
                cout << "EPOLLHUP" << endl;
            }
        }

	    /*
    int nKq, n;
    ICallBack* pCallBack;
    TVecKevent vecEvChange, vecEvList;
    vecEvList.resize( 1024 );

    timespec tmStep;
    tmStep.tv_sec = 0;
    tmStep.tv_nsec = 500000000;


    if( ( nKq = kqueue() ) < 0 )
        return errno;

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
		*/
	}
}
