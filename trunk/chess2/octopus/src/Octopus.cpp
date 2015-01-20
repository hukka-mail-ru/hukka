//============================================================================
// Name        : Octopus.cpp
// Author      : 
// Version     :
// Copyright   : Your copyright notice
// Description : Hello World in C++, Ansi-style
//============================================================================

#include <vector>
#include <list>
#include <iostream>

#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/ioctl.h>

#include "MyException.h"
#include "Log.h"

using namespace std;

const int MaxQueueSize = 100;
const int INVALID_SOCKET = -1;
const int SOCKET_ERROR = -1;

long bytesAvailable (int sock)
{
    long int avail = 0;
    size_t bytes;

    if (ioctl (sock, FIONREAD, (char *) &bytes) >= 0)
        return avail = (long int) *((int *) &bytes);

    return 0;
}

void parseReceivedBytes(const vector<unsigned char>& buffer)
{
	PRINT_LOG <<  "Received: ";
	string bytes(buffer.begin(), buffer.end());
	Log::WriteBytes(bytes);
	PRINT_LOG << "[" << bytes << "]\n";
}


int startServer(int port)
{
	int serverSocket = socket(AF_INET, SOCK_STREAM, IPPROTO_IP);
	if (serverSocket < 0)
	{
		THROW_EX(MyException()) << "Can't create socket";
	}




	// make server socket unblocking
/*	int flags = fcntl(serverSocket, F_GETFL, 0);  // bereits gesetzte Flags auslesen

	if (fcntl(serverSocket, F_SETFL, flags | O_NONBLOCK) < 0)  // socket auf non-blocking setzen
	{
		THROW_EX(MyException()) << "error setting socket to non blocking\n";

	}
*/
	sockaddr_in localaddr;
	localaddr.sin_addr.s_addr = htonl(INADDR_ANY);
	localaddr.sin_family = AF_INET;
	localaddr.sin_port = htons(port);

	if (bind(serverSocket, (sockaddr*)&localaddr, sizeof(localaddr)) < 0)
	{
		THROW_EX(MyException()) << "Can't bind socket";
	}

	listen(serverSocket, MaxQueueSize);
	PRINT_LOG << "Listen on port " << port << " socket: " << serverSocket << "\n";

	list<int> clientSockets;
	for(;;)
	{
		fd_set readSet;
		FD_ZERO(&readSet);
		FD_SET(serverSocket, &readSet);
		int max = serverSocket;
		for (auto clientSocket  = clientSockets.begin();
				  clientSocket != clientSockets.end(); clientSocket++)
		{
			FD_SET(*clientSocket, &readSet);
			if(*clientSocket > max)
				max = *clientSocket;
		}

		int readySocket = select(max + 1, &readSet, NULL, 0, NULL);


		if (readySocket < 0)
		{
			PRINT_LOG << "Select failed \n";
		}

		//We have new connection (Есть новые подключения)
		if (FD_ISSET(serverSocket, &readSet))
		{
			sockaddr_in clientaddr;
			socklen_t size = sizeof(clientaddr);
			int clientSocket = accept(serverSocket, (sockaddr*)&clientaddr, &size);
			clientSockets.push_back(clientSocket);

			if (clientSocket < 0)
			{
				PRINT_LOG << "Accept failed \n";
				break;
			}

			PRINT_LOG << "Client connected: " << clientSocket << "\n";
		}

		//We have data from client (Есть данные от клиента)
		for (auto clientSocket  = clientSockets.begin();
				  clientSocket != clientSockets.end(); clientSocket++)
		{
			if (*clientSocket == INVALID_SOCKET )
				continue;

			if (FD_ISSET(*clientSocket, &readSet))
			{
				int avail = bytesAvailable(*clientSocket);


				vector<unsigned char> buffer(avail);
				PRINT_LOG << "Avail. " << avail << " on socket " <<
						*clientSocket << " size "<< buffer.size() << "\n";


				int ret = recv(*clientSocket, &buffer[0], buffer.size(), 0);

				if (ret == 0)
				{
					close(*clientSocket);
					PRINT_LOG << "Client disconnected: " << *clientSocket << "\n";

					*clientSocket = INVALID_SOCKET;
					break;
				}
				else if (ret == SOCKET_ERROR)
				{
					PRINT_LOG <<  "Receive data failed \n";
					break;
				}

				parseReceivedBytes(buffer);

				//szRecvBuff[ret] = '\0';

				//strcpy(szSendBuff, "Command get OK");

				//ret = send(*clientSocket, szSendBuff,
				//	sizeof(szSendBuff), 0);
				//if (ret == SOCKET_ERROR)
				//{
				//	break;
				//}
			}


		}

		// get rid of INVALID SOCKETs
		clientSockets.remove_if([](int i) { return i == INVALID_SOCKET; });
	}

	close(serverSocket);
	return 0;

}


int main(int count, char *strings[])
{
	try
	{
	    if ( count != 2 )
	    {
	        printf("Usage: %s <portnum>\n", strings[0]);
	        exit(0);
	    }

		startServer(atoi(strings[1]));
	}
	catch (MyException& e)
	{
		PRINT_EX(e);
	}
}
