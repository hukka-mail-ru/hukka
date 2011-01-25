#ifndef _QUEUEMSG_H
#define _QUEUEMSG_H

#include <queue>
#include "clientmsg.h"

typedef std::queue<CClientMsg> TQueueMsg;

class CQueueMsg
{
public:

	CQueueMsg();
	~CQueueMsg();

	bool				AddMsg( const CClientMsg& );
	bool				GetMsg( CClientMsg& );

    int                 size() { m_queueMsg.size(); } 
private:

	pthread_mutex_t	m_mutQueue;

	TQueueMsg			m_queueMsg;

	bool				m_isGetEmpty;
};

#endif
