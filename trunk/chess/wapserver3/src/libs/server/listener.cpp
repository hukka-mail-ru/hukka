#include <errno.h>
#include <string.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <unistd.h>
#include "listener.h"
#include "selector.h"

#include <netinet/tcp.h>
#include <iostream>

using namespace std;

const int on = 1;
const int tm = 20;

CListener::CListener()
	:m_nSocket( -1 )
{
	m_pSRVServer = CSRVServer::Instance();
}

CListener::~CListener()
{
	Close();

	CSRVServer::FreeInst();
}

int CListener::Listen( uint16_t _nPort )
{
	m_nSocket = socket( AF_INET, SOCK_STREAM, 0 );

	if( m_nSocket < 0 )
		return errno;

	sockaddr_in addr;
	bzero( &addr,sizeof( addr ) );
	addr.sin_family = AF_INET;
	addr.sin_port = htons( _nPort );
	addr.sin_addr.s_addr = htonl( INADDR_ANY );

	setsockopt( m_nSocket, SOL_SOCKET, SO_REUSEADDR, &on, sizeof( on ) );
	setsockopt( m_nSocket, SOL_SOCKET, SO_KEEPALIVE, &on, sizeof( on ) );

	if( bind( m_nSocket, (sockaddr*)&addr, sizeof( addr ) ) != 0 )
		return errno;

	if( listen( m_nSocket, 1024 ) != 0 )
		return errno;

	cout << "CListener::Listen AddHandle" << endl;
    CSelector::Instance()->AddHandle( m_nSocket, EPOLL_CTL_ADD, EPOLLIN, static_cast<ICallBack*>( this ) );
	//CSelector::Instance()->AddHandle( m_nSocket, EVFILT_READ, EV_ADD, static_cast<ICallBack*>( this ) );
	CSelector::FreeInst();

	return 0;
}

int CListener::Close()
{
    cout << "Close socket : " << m_nSocket << endl;

	if( m_nSocket < 0 )
		return 0;

	shutdown( m_nSocket, SHUT_RDWR );
	close( m_nSocket );

	m_nSocket = -1;
}

int CListener::GetSocket() const
{
	return m_nSocket;
}

void CListener::DoRead()
{
    cout << "Read socket : " << m_nSocket << endl;

	sockaddr_in addr;
	socklen_t len = sizeof( addr );

	int nSocket = accept( m_nSocket, (sockaddr*)&addr, &len );

    cout << "Accept socket : " << nSocket << endl;

	if( nSocket != -1 )
		m_pSRVServer->AddSocket( nSocket, &addr );

}

void CListener::DoWrite()
{

}
