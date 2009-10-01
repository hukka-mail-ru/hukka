#ifndef _SELECTOR_H
#define _SELECTOR_H

#include <vector>
#include <sys/event.h>
#include "../header/interface.h"

typedef std::vector<struct kevent> TVecKevent;

class CSelector
{
public:

	static CSelector*		Instance();
	static void			FreeInst();
	static void			KillObject();

	void					AddHandle( int, short, u_short, ICallBack*, bool = false );

	int					StartLoop();
	void					StopLoop();
private:

	CSelector();
	virtual ~CSelector();

	int					MainLoop();
private:

	static CSelector*		m_pSelf;
	static int			m_nRefCount;

	pthread_mutex_t		m_mutLoop;
	pthread_mutex_t		m_mutAddE;

	TVecKevent			m_vecEvChange;

	bool					m_isContinue;
};

#endif
