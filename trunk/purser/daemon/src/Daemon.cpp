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


#include "Daemon.h"

using namespace std;

int Daemon::Daemonize()
{

	// make a child
	int pid = fork();

	if (pid == -1)
	{
		cout << "Error:  Start Daemon failed (%s)\n" << strerror(errno);
		return -1;
	}
	else if (!pid) // child
	{
		// daemon loop

		// rights for the files
		umask(0);

		// create a new session
		setsid();

		// change dir to root
		chdir("/");

		/* Ensure only one copy */
		int pidFilehandle = open(mPidfile.c_str(), O_RDWR|O_CREAT, 0600);
		if (pidFilehandle == -1 )
		{
			cout << "Could not open PID file " << mPidfile <<", exit" << endl;
			return -1;
		}

		/* Try to lock file */
		if (lockf(pidFilehandle,F_TLOCK,0) == -1)
		{
			cout << "Could not lock PID file " << mPidfile <<", exit" << endl;
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
		return Run();
	}
	else // parent: just ends
	{
		return 0;
	}


}
