#ifndef _MYSOCKET_H
#define _MYSOCKET_H

#include "socket.h"
#include "../tools/buffer.h"
#include "queuemsg.h"
#include "../header/interface.h"

class CClientMsg;
class CSocketManager;

class CMySocket : public ICallBack, public ISender, public CSocket
{
public:

	CMySocket( int _nSocket = -1, ISocketManager* = 0 );
	~CMySocket();
//ISender
	void				AddMsg( const CClientMsg& );
	bool				GetMsg( CClientMsg& );
private:
//ICallBack
	void				DoRead();
	void				DoWrite();

	void				DoClose();
private:

	CBuffer			m_Buffer;

   	CClientMsg      m_clientMsg;

	CQueueMsg			m_inQueueMsg;
	CQueueMsg			m_outQueueMsg;

	ISocketManager*	m_pSocketManager;
	

};

typedef std::queue<CMySocket*> TQueSocket;

#endif
