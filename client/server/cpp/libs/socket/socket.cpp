#include "socket.h"
#include <unistd.h>
#include <iostream>

CSocket::CSocket( int _nSocket )
	:m_nSocket( _nSocket )
{

}

CSocket::~CSocket()
{
	Close( SHUT_RDWR );
}

int CSocket::GetSocket() const
{
	return m_nSocket;
}

int CSocket::Recv( void * _pBuf, int _nSize )
{
	return recv( m_nSocket, _pBuf, _nSize, MSG_DONTWAIT );
}

int CSocket::Send( const void * _pBuf, int _nSize )
{
	int res = send( m_nSocket, _pBuf, _nSize, 0 );
	
#ifdef GMS_DEBUG	
	    std::cerr << "CSocket::Send() _nSize = " << _nSize << std::endl;
	    std::cerr << "CSocket::Send()  _pBuf = [ ";
		for(char* c = (char*)_pBuf; c !=  (char*)_pBuf + _nSize; ++c)
		{
			if ( (int) *c > 31)
				std::cerr << *c;
			else
				std::cerr << '?';
		}		
		std::cerr << " ]" << std::endl;	         
#endif
		
	return res;
}

int CSocket::Close( int _nHowTo )
{
	int nRes = 0;

#ifdef GMS_DEBUG
	std::cerr << "Socket::Close _nHowTo = " << _nHowTo << 
		         " m_nSocket =  " << m_nSocket << std::endl;
#endif 
	if( m_nSocket != -1 )
		nRes = shutdown( m_nSocket, _nHowTo );

	if( _nHowTo == SHUT_RDWR )
		m_nSocket = -1;

	return nRes;
}
