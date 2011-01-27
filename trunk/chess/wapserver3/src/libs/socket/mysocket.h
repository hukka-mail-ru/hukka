#ifndef _MYSOCKET_H
#define _MYSOCKET_H

#include "socket.h"
#include "../tools/buffer.h"
#include "queuemsg.h"
#include "../header/interface.h"

class ClientMsg;
class SocketManager;

class MySocket : public ICallBack, public ISender, public Socket
{
public:

	MySocket( int _nSocket = -1, ISocketManager* = 0 );
	~MySocket();
//ISender
	void				AddMsg( const ClientMsg& );
	bool				GetMsg( ClientMsg& );
private:
//ICallBack
	void				DoRead();
	void				DoWrite();

	void				DoClose();
private:

	Buffer			m_Buffer;

   	ClientMsg      m_clientMsg;

	QueueMsg			m_inQueueMsg;
	QueueMsg			m_outQueueMsg;

	ISocketManager*	m_pSocketManager;
	

};

typedef std::queue<MySocket*> TQueSocket;

#endif
