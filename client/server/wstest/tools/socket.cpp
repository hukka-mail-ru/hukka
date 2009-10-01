
#include "socket.h"

#include <errno.h>
#include <netinet/in.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <sys/socket.h>
#include <iostream>

CSocket::CSocket()
{
    
    m_nHandle = socket( AF_INET, SOCK_STREAM, 0 );
    if (m_nHandle < 0)
    {
    	std::cout << "CSocket::CSocket() error: m_nHandle = " << m_nHandle << std::endl;
    }
}

CSocket::CSocket( int _nHandle ) : m_nHandle( _nHandle )
{
    
};

int CSocket::getNewHandle()
{
    if ( m_nHandle != 0 )
    {
    	std::cout << "CSocket::getNewHandle()" << std::endl;
    	sockclose( m_nHandle );
    }
    return m_nHandle = socket( AF_INET, SOCK_STREAM, 0 );    
}

int CSocket::GetHandle() const
{
    
    return m_nHandle;
    
}

int CSocket::Send( const void* _pBuf, int _nSize )
{
    char* pTmp = ( char* ) _pBuf;
    return send( m_nHandle, _pBuf, _nSize, 0 );
}

int CSocket::Recv( void* _pBuf, int _nSize )
{
    return recv( m_nHandle, _pBuf, _nSize, MSG_DONTWAIT );
}

void CSocket::checkdata()
{
    bool isBuffFull;
    int nErr;
    
    do
    {

        int nSize = Recv( m_Buffer.GetDataEnd(), m_Buffer.FreeSize() );
        
#ifdef GMS_DEBUG
		std::cerr << "CSocket::checkdata() nSize = " << nSize << " m_Buffer.FreeSize()  = " << m_Buffer.FreeSize() << std::endl;
	    std::cerr << "CSocket::checkdata()  m_Buffer = [ ";
		for(char* c = m_Buffer.GetDataEnd(); c !=  m_Buffer.GetDataEnd() + nSize; ++c)
		{
			if ( (int) *c > 31)
				std::cerr << *c;
			else
				std::cerr << '?';
		}		
		std::cerr << " ]" << std::endl;	  
#endif //GMS_DEBUG
		
        nErr = errno;

        if( nSize > 0 )
        {
            m_Buffer.AddDataSize( nSize );

            if( isBuffFull = ( m_Buffer.FreeSize() == 0 ) )
                m_Buffer.IncBuffer();
        } 
        else if( ( nSize  == 0 ) || ( ( nSize < 0 ) && ( nErr != EAGAIN ) && ( nErr != EINTR ) ) )
        {
            //	std::cout << "CSocket::checkdata() : nSize = " << nSize << " nErr = " << nErr << std::endl;
            //sockclose();
            return;
        }
    
    } while( isBuffFull || ( nErr == EINTR ) );
    
    newdata();
    
}

int CSocket::GetData( void* _pBuf, int _nSize )
{
    if ( m_Buffer.DataSize() == 0 )
        return 0;
    
    if ( _nSize < m_Buffer.DataSize() )
        return -1;
    
    memcpy( _pBuf, m_Buffer.GetDataStart(), m_Buffer.DataSize() );
   
    int nSize = m_Buffer.DataSize();

    m_Buffer.RemoveData( m_Buffer.GetDataEnd() );

    return nSize;
}

void CSocket::wait()
{
    
    fd_set rset;
    
    FD_ZERO( &rset );
    FD_SET( m_nHandle, &rset );
    select( m_nHandle+1, &rset, NULL, NULL, NULL );
    
    checkdata();
    
}

///TODO доработать фукнцию с целью избавления от static char str[128]

char* CSocket::sock_ntop( const struct sockaddr* _sa, socklen_t _salen )
{
    char strPort[ 7 ];
    static char str[ 128 ];  
    
    switch ( _sa->sa_family )
    {
        case AF_INET:
        {
            struct sockaddr_in *sin = ( struct sockaddr_in *) _sa;
            
            if ( inet_ntop( AF_INET, &sin->sin_addr, str, sizeof( str ) ) == NULL )
                return ( NULL );
            
            if ( ntohs( sin->sin_port ) != 0 )
            {
                snprintf( strPort, sizeof( strPort ), ".%d", ntohs( sin->sin_port ) );
                strcat( str, strPort );
            }
            
        }
  
    }
    return str;
}

void CSocket::sockclose( int _nHandle )
{
	if (_nHandle)
    {
	    close( _nHandle );
    	disconnected();
    	m_nHandle = 0;
    }
}

void CSocket::sockclose()
{
    sockclose( m_nHandle );
}

CSocket::~CSocket()
{
    
}
