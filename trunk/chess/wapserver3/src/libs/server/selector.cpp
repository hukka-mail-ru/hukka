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
#include <stdlib.h>
#include <sys/time.h>


using namespace std;

Selector* Selector::m_pSelf = 0;
int Selector::m_nRefCount = 0;

Selector* Selector::Instance()
{
	if( m_pSelf == 0 )
		m_pSelf = new Selector;

	++m_nRefCount;

	return m_pSelf;
}

void Selector::FreeInst()
{
	--m_nRefCount;

	if( m_nRefCount > 0 )
		return;

	KillObject();
}

void Selector::KillObject()
{
	if( m_pSelf != 0 )
		delete m_pSelf;

	m_pSelf = 0;
	m_nRefCount = 0;
}

void Selector::AddReadHandle( int _Socket, IReaderWriter* _pReaderWriter)
{
    struct epoll_event ev;
    ev.events = EPOLLIN | EPOLLET;
    ev.data.ptr = _pReaderWriter;

    int res = epoll_ctl(m_epfd, EPOLL_CTL_ADD, _Socket, &ev);

    if(res != 0 && errno == EEXIST) // if already exists...
    {
        int res = epoll_ctl(m_epfd, EPOLL_CTL_MOD, _Socket, &ev);

        if(res != 0)
        {
            cout << "Selector::AddReadHandle. epoll_ctl failed" << endl;
            exit(1);
        }
    }
}

void Selector::AddWriteHandle( int _Socket, IReaderWriter* _pReaderWriter)
{
    struct epoll_event ev;
    ev.events = EPOLLOUT | EPOLLONESHOT;
    ev.data.ptr = _pReaderWriter;

    int res = epoll_ctl(m_epfd, EPOLL_CTL_ADD, _Socket, &ev);

    if(res != 0 && errno == EEXIST) // if already exists...
    {
        int res = epoll_ctl(m_epfd, EPOLL_CTL_MOD, _Socket, &ev);

        if(res != 0)
        {
            cout << "Selector::AddWriteHandle. epoll_ctl failed" << endl;
            exit(1);
        }
    }

    pthread_mutex_lock( &m_mutWritingSocket );
    m_WritingSocket = _Socket;
    pthread_mutex_unlock( &m_mutWritingSocket );

}

void Selector::RemoveHandle( int _Socket, IReaderWriter* _pReaderWriter)
{

    struct epoll_event ev;
    ev.events = EPOLLIN | EPOLLOUT;
    ev.data.ptr = _pReaderWriter;

    int res = epoll_ctl(m_epfd, EPOLL_CTL_DEL, _Socket, &ev);
    if(res != 0)
     {
         cout << "Selector::RemoveHandle. epoll_ctl failed" << endl;
         exit(1);
     }

#ifdef LOW_LEVEL_DEBUG
    cout << "SOCKET " << _Socket << " Selector::RemoveHandle. HANDLE REMOVED (epoll_ctl) " << endl;
#endif

}

int Selector::StartLoop()
{
	if( pthread_mutex_trylock( &m_mutLoop ) == EBUSY )
		return 0;

	int nRes = MainLoop();

	pthread_mutex_unlock( &m_mutLoop );

	return nRes;
}

void Selector::StopLoop()
{
	m_isContinue = false;

	kill( getpid(), SIGUSR1 );
}

#define MAX_LISTEN_PORT_NUM 1024
#define MAX_EPOLL_EVENTS_PER_RUN 1024
#define EPOLL_ETERNAL_WAIT -1 //milliseconds

Selector::Selector():
	 m_isContinue( false )
{

    pthread_mutex_init(&this->m_mutLoop, NULL);
    pthread_mutex_init(&this->m_mutAddE, NULL);
    pthread_mutex_init(&this->m_mutFd, NULL);
    pthread_mutex_init(&this->m_mutWritingSocket, NULL);

	signal( SIGUSR1, SIG_IGN );

	pthread_mutex_lock( &m_mutFd );
	m_epfd = epoll_create(MAX_LISTEN_PORT_NUM);
	assert(m_epfd >= 0);
	pthread_mutex_unlock( &m_mutFd );

}

Selector::~Selector()
{

}

int Selector::MainLoop()
{

	m_isContinue = true;

#ifdef LOW_LEVEL_DEBUG
    cout << "Selector::MainLoop. WAITING FOR EVENTS (epoll_wait)" << endl;
#endif

	while( m_isContinue )
	{
//	    cout << tm->tm_sec << "." << tv.tv_usec << " WAITING FOR EVENTS " << endl;
	    epoll_event events[MAX_EPOLL_EVENTS_PER_RUN];

        int nfds = epoll_wait(m_epfd, events, MAX_EPOLL_EVENTS_PER_RUN, EPOLL_ETERNAL_WAIT);

        if(nfds < 0)
        {
            cout << "epoll_wait error" << endl;
            return -1;
        }

        for(int i = 0; i < nfds; i++)
        {
            if(events[i].events & EPOLLIN)
            {
                static_cast<IReaderWriter*>(events[i].data.ptr)->DoRead();
            }
            else if(events[i].events & EPOLLOUT)
            {
                static_cast<IReaderWriter*>(events[i].data.ptr)->DoWrite();
//                cout << "Rearm" << endl;
                AddReadHandle(m_WritingSocket, static_cast<IReaderWriter*>(events[i].data.ptr));
            }
            else if(events[i].events & EPOLLPRI)
            {
                cout << "Selector::MainLoop. EPOLLPRI" << endl;
            }
            else if(events[i].events & EPOLLERR)
            {
                cout << "Selector::MainLoop. EPOLLERR" << endl;
            }
            else if(events[i].events & EPOLLHUP)
            {
                cout << "Selector::MainLoop. EPOLLHUP" << endl;
            }
            else
            {
                cout << "Selector::MainLoop. UNDEFINED EVENT" << endl;
            }
        }

	}

}
