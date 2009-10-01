#ifndef _CLIENTSOCKET_H
#define _CLIENTSOCKET_H

#include "../server/reginfo.h"
#include "mysocket.h"

class CClientSocket : public CMySocket, public CRegInfo
{
public:

	CClientSocket( int, ISocketManager*  );
	virtual ~CClientSocket();
};

#endif
