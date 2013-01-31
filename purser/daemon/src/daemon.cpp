//============================================================================
// Name        : daemon.cpp
// Author      : 
// Version     :
// Copyright   : Your copyright notice
// Description : Hello World in C++, Ansi-style
//============================================================================

#include <fcntl.h>
#include <errno.h>
#include <string.h>
#include <stdlib.h>
#include <sys/socket.h>
#include <sys/stat.h>
#include <arpa/inet.h>

#include <iostream>
#include <sstream>
#include <fstream>
#include <string>


#include "Socket.h"
#include "Log.h"
#include "Message.h"

using namespace std;



// LISTEN
int Run(int listener)
{
	listen(listener, 1);

	while (true)
	{
		int client = accept(listener, NULL, NULL);

		if (client < 0) {
			std::cout << "Error socket accept\n";
			return 1;
		}

		// Get from network
		Message mes = Socket::ReceiveMessage(client);

		Message reply;
		reply.setPhone("+79119089209");
		reply.setText("This is a normal reply");
		Socket::SendMessage(client, reply);

/*	    if (mes.substr(0, 3) == "GET")
		{
			SendBytes(client, "Reply to GET command \n");
		}*/

		close(client);
	}

	close(listener);

	return 0;
}


int main(int argc, char** argv)
{
	// READ COMMAND LINE
    string pidfile = "/var/run/daemon.pid";
    int port = 1234;

	for (int i = 0; i < argc; i++)
	{
		string arg = argv[i];

		if (arg == "--pidfile" && i+1 < argc)
		{
			pidfile = argv[i+1];
		}
		if (arg == "--logfile" && i+1 < argc)
		{
			Log::SetLogFile(argv[i+1]);
		}
		else if (arg == "--port" && i+1 < argc)
		{
			port = atoi(argv[i+1]);
		}
	}


	// PREPARE SOCKET
   int listener = socket(AF_INET, SOCK_STREAM, 0);

	if (listener < 0) {
		std::cout << "Error socket create\n";
		cout << strerror(errno) << "\n";
		return 1;
	}

	struct sockaddr_in addr;
	addr.sin_family = AF_INET;
	addr.sin_port = htons(port);
	addr.sin_addr.s_addr = htonl(INADDR_ANY);

	const int MAX_BIND_ATTEMPTS = 100;
	bool please_wait = true;

	for(int i=1; i<=MAX_BIND_ATTEMPTS; i++)
	{
		int res = bind(listener, (struct sockaddr*)&addr, sizeof(addr));
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
				cout << "Error socket bind\n";
				cout << strerror(errno) << "\n";
				return 1;
			}
		}
		else
		{
			break;
		}
	}

	cout << "Ready: port " << port << endl;



    // DAEMONIZE
	Log::Clear();

    // make a child
    int pid = fork();

    if (pid == -1)
    {
    	cout << "Error: Start Daemon failed (%s)\n" << strerror(errno);
        return -1;
    }
    else if (!pid) // child
    {
    	// rights for the files
        umask(0);

        // create a new session
        setsid();

        // change dir to root
        chdir("/");

        /* Ensure only one copy */
        int pidFilehandle = open(pidfile.c_str(), O_RDWR|O_CREAT, 0600);
        if (pidFilehandle == -1 )
        {
            cout << "Could not open PID file " << pidfile <<", exit" << endl;
            return -1;
        }

		/* Try to lock file */
		if (lockf(pidFilehandle,F_TLOCK,0) == -1)
		{
		    cout << "Could not lock PID file " << pidfile <<", exit" << endl;
		    return -1;
		}

		// write PID to file
		char str[10];
        sprintf(str,"%d\n", getpid());
        write(pidFilehandle, str, strlen(str));

        // close descriptors
        close(STDIN_FILENO);
        close(STDOUT_FILENO);
        close(STDERR_FILENO);

        // daemon loop
        return Run(listener);
    }
    else // parent: just ends
    {
        return 0;
    }
}
