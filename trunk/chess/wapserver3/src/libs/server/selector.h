#ifndef _SELECTOR_H
#define _SELECTOR_H

#include <vector>
//#include <sys/event.h>
#include <sys/epoll.h>
#include "../header/interface.h"

typedef std::vector<struct kevent> TVecKevent;

class Selector
{
public:

	static Selector*		Instance();
	static void			FreeInst();
	static void			KillObject();

	void AddHandle   ( int _Socket, unsigned _Flags, ICallBack* _pCallBack);
	void RemoveHandle( int _Socket, unsigned _Flags, ICallBack* _pCallBack);

	int					StartLoop();
	void					StopLoop();
private:

	Selector();
	virtual ~Selector();

	int					MainLoop();
private:

	static Selector*		m_pSelf;
	static int			m_nRefCount;

	pthread_mutex_t		m_mutLoop;
	pthread_mutex_t		m_mutAddE;

//	TVecKevent			m_vecEvChange;

	bool					m_isContinue;

	pthread_mutex_t        m_mutFd;
	int m_epfd;

	int m_WritingSocket;
};

#endif
