#ifndef _SOCKETMANAGER_H
#define _SOCKETMANAGER_H

#include <semaphore.h>
#include "../tools/mythread.h"
#include "../socket/mysocket.h"
#include "selector.h"

class SocketManager : public ISocketManager, private CMyThread
{
public:

	SocketManager( int = 1 );
	~SocketManager();
// ISocketManager
	void				RemoveSocket( MySocket* );
	void				AddInMsg( MySocket* );
	void				AddOutMsg( MySocket* );
protected:

	Selector*		m_pSelector;
private:

	virtual void		DoAllMsg( MySocket* ) = 0;

	int				Run();
private:

	sem_t			m_semDeqSocket;
	pthread_mutex_t	m_mutDeqSocket;

	TQueSocket		m_queSocket;
};

#endif
