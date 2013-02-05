#include <fcntl.h>
#include <errno.h>
#include <stdlib.h>

#include <iostream>
#include <string>


#include "Receiver.h"
#include "Log.h"
#include "MyException.h"

using namespace std;


int main(int argc, char** argv)
{
	try
	{
		string logfile = "/var/log/receiver.log";

		// READ COMMAND LINE
		string pidfile = "/var/run/receiver.pid";
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
				logfile = argv[i+1];
			}
			else if (arg == "--port" && i+1 < argc)
			{
				port = atoi(argv[i+1]);
			}
		}

		Log::SetLogFile(logfile);

		Receiver receiver(pidfile);

		receiver.ListenPort(port);

		cout << "Log: " << logfile << endl;
		cout << "Ready - port " << port << endl;

		// daemon loop
		return receiver.Daemonize();
	}
	catch (MyException& e)
	{
		cout << e.what() << endl;
		return -1;
	}
}
