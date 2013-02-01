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


using namespace std;

void Socket::ConnectToHost(const string& host, unsigned port)
{
	// PREPARE SOCKET
	struct hostent     *he;
	struct sockaddr_in  server;

	mSockfd = socket(AF_INET, SOCK_STREAM, 0);
	if (mSockfd==-1)
	{
		perror("Can't Create socket");
	}

	cout << "socket ready" << endl;

	/* resolve host */
	if ((he = gethostbyname(host.c_str())) == NULL)
	{
		puts("error resolving hostname..");
		exit(1);
	}

	cout << "gethostbyname" << endl;

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
		puts("error connecting..");
		exit(1);
	}

	cout << "connected" << endl;

	Log::Clear();
	Log::SetLogFile("/home/hukka/devel/purser/sender/Debug/sender_log.txt");

	cout << "done" << endl;
}

void Socket::Listen(int port)
{
	// PREPARE SOCKET
    mListener = socket(AF_INET, SOCK_STREAM, 0);

	if (mListener < 0)
	{
		//throw
		Log::Write("Error socket create");
		return;
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
				// throw
				Log::Write("Error socket bind");
				Log::Write(strerror(errno));
				return;
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
		Log::Write("Error socket accept");
	}
}

Message Socket::ReceiveMessage() const
{
	char buf[MESSAGE_SIZE] = {'0'};
	recv(mSockfd, buf, MESSAGE_SIZE, MSG_WAITALL);

	Message mes = Message::Parse(buf);

	return mes;
}


void Socket::SendMessage(const Message& mes) const
{
	string str = mes.Serialize();

	send(mSockfd, str.c_str(), str.length(), 0);
}

void Socket::Close() const
{
	close(mSockfd);
}

void Socket::StopListen() const
{
	close(mListener);
}
