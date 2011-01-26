#ifndef _CLIENTSOCKET_H
#define _CLIENTSOCKET_H

#include "../server/reginfo.h"
#include "mysocket.h"

class ClientSocket : public MySocket, public RegInfo
{
public:

	ClientSocket( int, ISocketManager*  );
	virtual ~ClientSocket();
};

#endif
