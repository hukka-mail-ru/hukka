#ifndef _SOCKETMANAGER_H
#define _SOCKETMANAGER_H

#include <semaphore.h>
#include "../tools/mythread.h"
#include "../socket/mysocket.h"
#include "selector.h"

class CSocketManager : public ISocketManager, private CMyThread
{
public:

	CSocketManager( int = 1 );
	~CSocketManager();
// ISocketManager
	void				OnClose( CMySocket* );
	void				AddInMsg( CMySocket* );
	void				AddOutMsg( CMySocket* );
protected:

	CSelector*		m_pSelector;
private:

	virtual void		DoAllMsg( CMySocket* ) = 0;

	int				Run();
private:

	sem_t			m_semDeqSocket;
	pthread_mutex_t	m_mutDeqSocket;

	TQueSocket		m_queSocket;
};

#endif
