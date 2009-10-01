#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <iostream>
#include "../libs/socket/clientmsg.h"
#include "../libs/header/defserver.h"
#include "../libs/header/deferror.h"
#include <poll.h>

using namespace std;

int main(int argc, char *argv[])
{
	char cL[] = "boon";
	char cP[] = "test";
	char cM[] = "Test message!!!";

	int sockfd = socket( AF_INET, SOCK_STREAM, 0 );

	struct sockaddr_in servaddr;
	bzero( &servaddr, sizeof( servaddr ) );
	servaddr.sin_family = AF_INET;
	servaddr.sin_port = htons( 1234 );
	inet_pton( AF_INET, "82.146.42.150", &servaddr.sin_addr );
//	inet_pton( AF_INET, "192.168.1.11", &servaddr.sin_addr );
//	inet_pton( AF_INET, "192.168.1.14", &servaddr.sin_addr );

	connect( sockfd, (sockaddr*)&servaddr, sizeof( servaddr ) );

	CClientMsg clientMsg;
	TVecChar vecData;
	vecData.assign( strlen( cL )+1+strlen( cP ), 0 );
	memcpy( &vecData[0], cL, strlen( cL ) );
	memcpy( &vecData[strlen( cL )+1], cP, strlen( cP ) );
	clientMsg.InitMsg( 1, 1, vecData );

	const TVecChar* pvecMsg = clientMsg.GetBuffMsg();
	TVecChar vecMsg;
	vecMsg.assign( &(*pvecMsg)[0], &(*pvecMsg)[0]+pvecMsg->size() );
//	vecMsg.assign( 20, 'a' );
//	vecMsg[vecMsg.size()-1] = 10;

	send( sockfd, &vecMsg[0], vecMsg.size(), 0 );

	pollfd pFd;

	pFd.fd = sockfd;
	pFd.events = POLLIN;

	poll( &pFd, 1, -1 );

	CBuffer Buffer;
	Buffer.AddDataSize( recv( sockfd, Buffer.GetDataEnd(), Buffer.FreeSize(), MSG_DONTWAIT ) );

	int nErr;

	if( clientMsg.ParseData( &Buffer, nErr ) )
	{
		TVecChar vecData;
		clientMsg.GetData( CClientMsg::etpCommand, &vecData );

		switch ( vecData[0] )
		{
		case NOERR:		std::cout << "OK" << std::endl;			break;
		case ERRBADLOGIN:	std::cout << "ErrLogin" << std::endl;		break;
		case ERRBADPASS:	std::cout << "ErrPassWord" << std::endl;	break;
		case ERRUSERONLINE:	std::cout << "ErrLoginOnLine" << std::endl;	break;	
		default:			std::cout << "Err???" << std::endl;		break;
		}
	}

	vecData.assign( strlen( cM )+1, 0 );
	memcpy( &vecData[0], cM, strlen( cM ) );
	clientMsg.InitMsg( 3, 1, vecData );
	pvecMsg = clientMsg.GetBuffMsg();
	vecMsg.assign( &(*pvecMsg)[0], &(*pvecMsg)[0]+pvecMsg->size() );
	send( sockfd, &vecMsg[0], vecMsg.size(), 0 );

	poll( &pFd, 1, -1 );

	Buffer.AddDataSize( recv( sockfd, Buffer.GetDataEnd(), Buffer.FreeSize(), MSG_DONTWAIT ) );

	if( clientMsg.ParseData( &Buffer, nErr ) )
	{
		TVecChar vecData;
		clientMsg.GetData( CClientMsg::etpCommand, &vecData );

		std::cout << endl;

		for( TVecChar::const_iterator cIt = vecData.begin() ; cIt != vecData.end() ; ++cIt )
			std::cout << *cIt;

		std::cout << endl;
	}

	close( sockfd );

	return EXIT_SUCCESS;
}
