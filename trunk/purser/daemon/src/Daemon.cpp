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
#include "MyException.h"
#include "Base.h"

using namespace std;

void Daemon::ReadConfigFile(const string& configfile)
{

	ifstream file;
	file.open(configfile.c_str());

	if(!file)
	{
		THROW_EX(MyException()) << "Can't open config file: " << configfile;
	}

	string line;
	while( getline(file, line) )
	{
	  istringstream iss_line(line);
	  string key;
	  if( getline(iss_line, key, '=') )
	  {
	    string value;
	    if( getline(iss_line, value) )
	    {
	    	mConfig[key] = value;
	    }
	  }
	}
}

std::string Daemon::GetConfigValue(const std::string& key)
{
	return mConfig[key];
}

int Daemon::Daemonize()
{

	// make a child
	int pid = fork();

	if (pid == -1)
	{
		THROW_EX(MyException()) << " Start Daemon failed: " << strerror(errno);
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
			THROW_EX(MyException()) << "Could not open PID file " << mPidfile <<", exit\n";
		}

		/* Try to lock file */
		if (lockf(pidFilehandle,F_TLOCK,0) == -1)
		{
			THROW_EX(MyException()) << "Could not lock PID file " << mPidfile <<", exit\n";
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
