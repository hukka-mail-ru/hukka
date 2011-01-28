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

Listener::Listener()
	:m_nSocket( -1 )
{
	m_pSRVServer = SRVServer::Instance();
}

Listener::~Listener()
{
	Close();

	SRVServer::FreeInst();
}

int Listener::Listen( uint16_t _nPort )
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

#ifdef LOW_LEVEL_DEBUG
	cout << "SOCKET " << m_nSocket <<  " Listener::Listen. OPEN SOCKET" << endl;
#endif

	if( listen( m_nSocket, 1024 ) != 0 ) // 1024 is maximum number of connections
		return errno;

#ifdef LOW_LEVEL_DEBUG
	cout << "SOCKET " << m_nSocket << " Listener::Listen. PREPARE TO ACCEPT CONNECTIONS... " << endl;
#endif
    Selector::Instance()->AddReadHandle( m_nSocket, static_cast<IReaderWriter*>( this ) );
	//Selector::Instance()->AddHandle( m_nSocket, EVFILT_READ, EV_ADD, static_cast<IReaderWriter*>( this ) );
	Selector::FreeInst();

	return 0;
}

int Listener::Close()
{
#ifdef LOW_LEVEL_DEBUG
    cout << "SOCKET " << m_nSocket << " Listener::Close. CLOSE SOCKET" << endl;
#endif

	if( m_nSocket < 0 )
		return 0;

	shutdown( m_nSocket, SHUT_RDWR );
	close( m_nSocket );

	m_nSocket = -1;
}

int Listener::GetSocket() const
{
	return m_nSocket;
}

void Listener::DoRead()
{
#ifdef LOW_LEVEL_DEBUG
    cout << "SOCKET " << m_nSocket << " Listener::DoRead. READ FROM SOCKET" << endl;
#endif

	sockaddr_in addr;
	socklen_t len = sizeof( addr );

	int nSocket = accept( m_nSocket, (sockaddr*)&addr, &len );

#ifdef LOW_LEVEL_DEBUG
    cout << "SOCKET " << m_nSocket << " Listener::DoRead. CONNECTED WITH SOCKET: " << nSocket << endl;
#endif

	if( nSocket != -1 )
		m_pSRVServer->AddSocket( nSocket, &addr );

}

void Listener::DoWrite()
{

}
