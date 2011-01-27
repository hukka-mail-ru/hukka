#ifndef _QUEUEMSG_H
#define _QUEUEMSG_H

#include <queue>
#include "clientmsg.h"

typedef std::queue<ClientMsg> TQueueMsg;

class QueueMsg
{
public:

	QueueMsg();
	~QueueMsg();

	bool				AddMsg( const ClientMsg& );
	bool				GetMsg( ClientMsg& );

    int                 size() { m_queueMsg.size(); } 
private:

	pthread_mutex_t	m_mutQueue;

	TQueueMsg			m_queueMsg;

	bool				m_isGetEmpty;
};

#endif
