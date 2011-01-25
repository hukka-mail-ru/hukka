#ifndef _SOCKET_H
#define _SOCKET_H

#include <sys/types.h>
#include <sys/socket.h>

class CSocket
{
public:
	
	CSocket( int _nSocket );
	virtual ~CSocket();
	
	int		GetSocket() const;
	
	int		Recv( void* _pBuf, int _nSize );
	int		Send( const void* _pBuf, int _nSize );
	
	int		Close( int ); // SHUT_RDWR SHUT_RD SHUT_WR
protected:

	int		m_nSocket;
};

#endif
