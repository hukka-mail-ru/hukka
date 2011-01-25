#include "clientsocket.h"

#include <sys/socket.h>
#include <arpa/inet.h>
#include <errno.h>
#include <netdb.h>
#include <netinet/in.h>
#include <iostream>
#include <sys/socket.h>
#include <string.h>


CClientSocket::CClientSocket() : m_bRunning( false )
{
}

bool CClientSocket::ConnectToIP( stlstr _strHost, int _nPort )
{
    struct sockaddr_in adrHost;
    bzero( &adrHost, sizeof( adrHost ) );
    adrHost.sin_family = AF_INET;
    adrHost.sin_port = htons( _nPort );

    if ( inet_pton( AF_INET, _strHost.c_str(), &adrHost.sin_addr ) != 1 )
        return false;

    int nRes = connect( m_nHandle,reinterpret_cast< const sockaddr* > ( &adrHost ), sizeof( adrHost ) );

    if ( nRes != 0 )
        return false;

    this->connected();

    this->Continue();

    return true;
}

bool CClientSocket::ConnectToHost( stlstr _strHostName, int _nPort )
{
    struct sockaddr_in adrHost;
    struct in_addr **pptr;
    struct hostent *hp;


    if ( ( hp = gethostbyname( _strHostName.c_str() ) ) == NULL )
    {
        std::cout << h_errno << std::endl;
        return false;
    }

    pptr = reinterpret_cast< struct in_addr ** > ( hp->h_addr_list );

    for( ; *pptr != NULL; pptr++ )
    {
        bzero( &adrHost, sizeof( adrHost ) );
        adrHost.sin_family = AF_INET;
        adrHost.sin_port = htons( _nPort );

        memcpy( &adrHost.sin_addr, *pptr, sizeof( struct in_addr ) );

        if ( sock_ntop( reinterpret_cast< const sockaddr* > ( &adrHost ), sizeof( adrHost ) ) == NULL )
            return false;

        int nRes = connect( m_nHandle,reinterpret_cast< const sockaddr* > ( &adrHost ), sizeof( adrHost ) );

        if ( nRes == 0 )
        {
            this->connected();

            this->Continue();

            return true;
        }

        std::cout << "CClientSocket::ConnectToHost: close m_nHandle" << m_nHandle << std::endl;
        close( m_nHandle );
        m_nHandle = getNewHandle();
    }

    return false;

}

void CClientSocket::newdata()
{
    CBuffer buf;
    GetData( buf.GetDataStart(), 255 );
    stlstr str( buf.GetDataStart() );

    std::cout << str << std::endl;
}

int CClientSocket::Run()
{
    m_bRunning = true;
    while( GetHandle() && m_bRunning )
        wait();
}

CClientSocket::~CClientSocket()
{

}
