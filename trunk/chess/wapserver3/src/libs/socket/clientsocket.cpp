#include <errno.h>
#include "clientsocket.h"
#include "../header/deferror.h"

#include <iostream>

CClientSocket::CClientSocket( int _nSocket, ISocketManager* _pSocketManager )
	:CMySocket( _nSocket, _pSocketManager )
{

}

CClientSocket::~CClientSocket()
{

}

