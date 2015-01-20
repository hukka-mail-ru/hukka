#include <sys/socket.h>
#include <fcntl.h>
#include <errno.h>
#include <string.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <arpa/inet.h>
#include <netdb.h>

#include <string>
#include <iostream>


#include "Log.h"
#include "MyException.h"

int mSockfd;

using namespace std;

void connectToHost(const string& host, unsigned port)
{
	// PREPARE SOCKET
	struct hostent     *he;
	struct sockaddr_in  server;

	mSockfd = socket(AF_INET, SOCK_STREAM, 0);
	if (mSockfd==-1)
	{
		THROW_EX(SocketException(host, port)) << "Can't create socket";
	}

	/* resolve host */
	if ((he = gethostbyname(host.c_str())) == NULL)
	{
		THROW_EX(SocketException(host, port)) << "Error resolving host name";
	}

	/*
	* copy the network address part of the structure to the
	* sockaddr_in structure which is passed to connect()
	*/
	memcpy(&server.sin_addr, he->h_addr_list[0], he->h_length);
	server.sin_family = AF_INET;
	server.sin_port = htons(port);

	/* connect */
	if (connect(mSockfd, (struct sockaddr *)&server, sizeof(server)))
	{
		THROW_EX(SocketException(host, port)) << "Connect error";
	}

	PRINT_LOG << "connected to host: " << host << ":" << port << "\n";
}

void disconnectFromHost()
{
	shutdown(mSockfd, SHUT_RDWR);
}



void receiveMessage()
{
	PRINT_LOG << " === INCOMING === \n";

	char buf[1024] = {'0'};
	recv(mSockfd, buf, 1024, MSG_WAITALL);


}


void sendMessage()
{
	PRINT_LOG << "======= OUTGOING  =======" << "\n";

	string str = "Four of the world's biggest tech giants - Apple, Google, Intel and Adobe - have agreed to a new settlement of $415m (£273.5m) in an attempt to resolve a lawsuit. The US lawsuit alleged the firms agreed not to poach staff from each other. It claimed the alleged agreement prevented workers from getting better job offers elsewhere. The 2011 US case had claimed $3bn in damages on behalf of more than 64,000 workers at the four firms.The latest attempt to settle the case for $415m, which was filed in court on Thursday, comes after a US judge rejected a $324.5m settlement offer last year."
"Executive emails The judge had deemed that settlement offer too low.If the companies had lost the case and damages were awarded, they could have tripled to some $9bn under US antitrust laws.The lawsuit, which was based largely on emails between some executives of the tech firms, has been watched with much interest for details about the alleged pact.According to some reports, one email exchange cited in the lawsuit shows Eric Schmidt, former chief executive of Google, telling Steve Jobs the former boss of Apple, that a Google recruiter who solicited an Apple employee would be fired.";

	int res = send(mSockfd, str.c_str(), str.length(), 0);
	PRINT_LOG << res << "\n";
}



int main()
{
	try
	{
		connectToHost("localhost", 9900);
		sendMessage();
	}
	catch (MyException& e)
	{
		PRINT_EX(e);
	}
}
