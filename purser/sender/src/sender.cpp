//============================================================================
// Name        : sender.cpp
// Author      : 
// Version     :
// Copyright   : Your copyright notice
// Description : Hello World in C++, Ansi-style
//============================================================================

#include <fcntl.h>
#include <errno.h>
#include <string.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/stat.h>
#include <arpa/inet.h>
#include <netdb.h>


#include <iostream>
#include <sstream>
#include <fstream>
#include <string>

#include <stdlib.h>

#include "Socket.h"
#include "Log.h"

using namespace std;


int main(int argc, char** argv)
{
	cout << "hello" << endl;

	// READ COMMAND LINE
    int port = 1234;

	for (int i = 0; i < argc; i++)
	{
		string arg = argv[i];

		if (arg == "--port" && i+1 < argc)
		{
			port = atoi(argv[i+1]);
		}
	}


	// PREPARE SOCKET
	 struct hostent     *he;
	 struct sockaddr_in  server;
	 int                 sockfd;

	 sockfd=socket(AF_INET,SOCK_STREAM,0);
	  if (sockfd==-1)
	    perror("Can't Create socket");

	  cout << "socket ready" << endl;

	/* resolve localhost to an IP (should be 127.0.0.1) */
	  if ((he = gethostbyname("localhost")) == NULL) {
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
	  server.sin_port = htons(1234);

	/* connect */
	  if (connect(sockfd, (struct sockaddr *)&server, sizeof(server)))
	  {
	    puts("error connecting..");
	    exit(1);
	  }

	    cout << "connected" << endl;

	    Log::Clear();
	    Log::SetLogFile("/home/hukka/devel/purser/sender/Debug/sender_log.txt");

	    cout << "done" << endl;

	    Message mes;
	    mes.setPhone("+79115361051");
	    mes.setText("Hello from home");

	    Socket::SendMessage(sockfd, mes);



}
