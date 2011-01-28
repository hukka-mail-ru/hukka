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

	void AddReadHandle ( int _Socket, IReaderWriter* _pReaderWriter);
    void AddWriteHandle( int _Socket, IReaderWriter* _pReaderWriter);
	void RemoveHandle  ( int _Socket, IReaderWriter* _pReaderWriter);

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
	pthread_mutex_t     m_mutWritingSocket;

//	TVecKevent			m_vecEvChange;

	bool					m_isContinue;

	pthread_mutex_t        m_mutFd;
	int m_epfd;

	int m_WritingSocket;
};

#endif
