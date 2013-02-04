/*
 * Client.cpp
 *
 *  Created on: 31 Jan 2013
 *      Author: hukka
 */

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

#include "Socket.h"
#include "Log.h"
#include "MyException.h"


using namespace std;

void Socket::ConnectToHost(const string& host, unsigned port)
{
	// PREPARE SOCKET
	struct hostent     *he;
	struct sockaddr_in  server;

	mSockfd = socket(AF_INET, SOCK_STREAM, 0);
	if (mSockfd==-1)
	{
		throw ExceptionSocketError(PRINT_WHERE, "Can't create socket", host, port);
	}

	/* resolve host */
	if ((he = gethostbyname(host.c_str())) == NULL)
	{
		throw ExceptionSocketError(PRINT_WHERE, "Error resolving host name", host, port);
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
		throw ExceptionSocketError(PRINT_WHERE, "Connect error", host, port);
	}

	cout << "connected to host" << endl;
}

void Socket::Listen(int port)
{
	// PREPARE SOCKET
    mListener = socket(AF_INET, SOCK_STREAM, 0);
    mPort = port;

	if (mListener < 0)
	{
		throw ExceptionSocketError(PRINT_WHERE, "Error creating socket", "-", port);
	}

	struct sockaddr_in addr;
	addr.sin_family = AF_INET;
	addr.sin_port = htons(port);
	addr.sin_addr.s_addr = htonl(INADDR_ANY);

	const int MAX_BIND_ATTEMPTS = 100;
	bool please_wait = true;

	for(int i=1; i<=MAX_BIND_ATTEMPTS; i++)
	{
		int res = bind(mListener, (struct sockaddr*)&addr, sizeof(addr));
		if (res < 0)
		{
			if(errno == EADDRINUSE && i < MAX_BIND_ATTEMPTS)
			{
				if(please_wait)
				{
					cout << "Binding to socket, please wait..." << endl;
					please_wait = false;
				}

				sleep(1);
				continue;
			}
			else
			{
				throw ExceptionSocketError(PRINT_WHERE, "Error binding socket", "-", port);
			}
		}
		else
		{
			break;
		}
	}

	listen(mListener, 1);
}


void Socket::Open()
{
	mSockfd = accept(mListener, NULL, NULL);

	if (mSockfd < 0)
	{
		// throw exception
		throw ExceptionSocketError(PRINT_WHERE, "Error opening socket", "-", mPort);
	}
}


Message Socket::ReceiveMessage() const
{
	PRINT_LOG << " === INCOMING === \n";

	char buf[MessageSize] = {'0'};
	recv(mSockfd, buf, MessageSize, MSG_WAITALL);

	Message mes(buf);

	PRINT_LOG << "MESSAGE_SIZE: " << MessageSize << "\n";
	PRINT_LOG << "mes.phone: "    << mes.GetPhone() << "\n";
	PRINT_LOG << "mes.text: "     << mes.GetText() << "\n";

	return mes;
}


void Socket::SendMessage(const Message& mes) const
{

	PRINT_LOG << "======= OUTGOING  =======" << "\n";

	string str = mes.Serialize();

	send(mSockfd, str.c_str(), str.length(), 0);

	PRINT_LOG << "Phone: " << mes.GetPhone() << "; Len " << mes.GetPhone().length() << "\n";
	PRINT_LOG << "Text: "  << mes.GetText() << "; Len " << mes.GetText().length() << "\n";
	Log::WriteBytes(str);
	PRINT_LOG << "Len: "   << str.length() << "\n";
}

void Socket::Close() const
{
	close(mSockfd);
}

void Socket::StopListen() const
{
	close(mListener);
}
