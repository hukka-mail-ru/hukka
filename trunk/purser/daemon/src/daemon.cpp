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
#include <sys/stat.h> // umask

#include <iostream>
#include <sstream>
#include <fstream>
#include <string>


#include "Socket.h"
#include "Log.h"
#include "Message.h"
#include "MyException.h"

using namespace std;


// LISTEN
int Run(Socket& socket)
{
	//Log::Write("RUN");
	try
	{
		while (true)
		{
			socket.Open();

			// Get from network
			Message mes = socket.ReceiveMessage();

			Message reply;
			reply.setPhone("+79119089209");
			reply.setText("This is a normal reply");
			socket.SendMessage(reply);

	/*	    if (mes.substr(0, 3) == "GET")
			{
				SendBytes(client, "Reply to GET command \n");
			}*/

			socket.Close();
		}

		socket.StopListen();
	}
	catch (MyException& e)
	{
		Log::Write(e.what());
		return -1;
	}

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


	Socket socket;
	try
	{
		socket.Listen(port);
	}
	catch (MyException& e)
	{
		cout << e.what() << endl;
		return -1;
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
		return Run(socket);
	}
	else // parent: just ends
	{
		return 0;
	}


}
