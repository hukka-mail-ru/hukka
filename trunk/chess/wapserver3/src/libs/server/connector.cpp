#include "connector.h"
#include <string.h>
#include <sys/socket.h>
#include <arpa/inet.h>
#include "selector.h"
#include <iostream>

using namespace std;

Connector::Connector( ISocketManager* _pISocketManager )
	:MySocket( -1, _pISocketManager )
{

}

Connector::~Connector()
{
#ifdef GMS_DEBUG
	std::cerr << "Connector::~Connector()" << std::endl;
#endif
	Close( SHUT_RDWR );
}

bool Connector::Connect( in_port_t _nPort, const char* _cAddr, const AccessInfo* _pAccessInfo )
{
	if( ( m_nSocket = socket( AF_INET, SOCK_STREAM, 0 ) ) < 1 )
		return false;

	struct sockaddr_in servaddr;
	bzero( &servaddr, sizeof( servaddr ) );
	servaddr.sin_family = AF_INET;
	servaddr.sin_port = htons( _nPort );
	inet_pton( AF_INET, _cAddr, &servaddr.sin_addr );

	if( connect( m_nSocket, (sockaddr*)&servaddr, sizeof( servaddr ) ) != 0 )
		return false;

	cout << "Connector::Connect AddHandle" << endl;
	Selector::Instance()->AddHandle( m_nSocket, EPOLLIN, static_cast<ICallBack*>( this ) );
	//Selector::Instance()->AddHandle( m_nSocket, EVFILT_READ, EV_ADD, static_cast<ICallBack*>( this ) );
	Selector::FreeInst();

	Register( _pAccessInfo );

	return true;
}

bool Connector::Register( const AccessInfo* _pAccessInfo )
{
	const char* pLogin = _pAccessInfo->GetLogin();
	const char* pPassword = _pAccessInfo->GetPassword();

	ClientMsg clientMsg;
	TVecChar vecData;
	vecData.assign( strlen( pLogin )+1+strlen( pPassword ), 0 );
	memcpy( &vecData[0], pLogin, strlen( pLogin ) );
	memcpy( &vecData[strlen( pLogin )+1], pPassword, strlen( pPassword ) );
	clientMsg.InitMsg( 1, 1, vecData );

	const TVecChar* pvecMsg = clientMsg.GetBuffMsg();
	TVecChar vecMsg;
	vecMsg.assign( &(*pvecMsg)[0], &(*pvecMsg)[0]+pvecMsg->size() );

	Send( &vecMsg[0], vecMsg.size() );

	return true;
}
