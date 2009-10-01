#include "connector.h"
#include <string.h>
#include <sys/socket.h>
#include <arpa/inet.h>
#include "selector.h"
#include <iostream>

CConnector::CConnector( ISocketManager* _pISocketManager )
	:CMySocket( -1, _pISocketManager )
{

}

CConnector::~CConnector()
{
#ifdef GMS_DEBUG
	std::cerr << "CConnector::~CConnector()" << std::endl;
#endif 
	Close( SHUT_RDWR );
}

bool CConnector::Connect( in_port_t _nPort, const char* _cAddr, const CAccessInfo* _pAccessInfo )
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

	CSelector::Instance()->AddHandle( m_nSocket, EVFILT_READ, EV_ADD, static_cast<ICallBack*>( this ) );
	CSelector::FreeInst();

	Register( _pAccessInfo );

	return true;
}

bool CConnector::Register( const CAccessInfo* _pAccessInfo )
{
	const char* pLogin = _pAccessInfo->GetLogin();
	const char* pPassword = _pAccessInfo->GetPassword();

	CClientMsg clientMsg;
	TVecChar vecData;
	vecData.assign( strlen( pLogin )+1+strlen( pPassword ), 0 );
	memcpy( &vecData[0], pLogin, strlen( pLogin ) );
	memcpy( &vecData[strlen( pLogin )+1], pPassword, strlen( pPassword ) );
	clientMsg.InitMsg( 1, 1, vecData );

	const TVecChar* pvecMsg = clientMsg.GetBuffMsg();
	TVecChar vecMsg;
	vecMsg.assign( &(*pvecMsg)[0], &(*pvecMsg)[0]+pvecMsg->size() );

	Send( &vecMsg[0], vecMsg.size() );
}
