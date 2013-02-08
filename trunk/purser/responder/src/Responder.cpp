//============================================================================
// Name        : daemon.cpp
// Author      : 
// Version     :
// Copyright   : Your copyright notice
// Description : Hello World in C++, Ansi-style
//============================================================================


#include "Responder.h"
#include "MyException.h"
#include "Log.h"

using namespace std;

Responder::Responder(const std::string& pidfile, const std::string& configfile):
	Daemon(pidfile, configfile)
{
	int inport = atoi(GetConfigValue("inport").c_str());
	string logfile = GetConfigValue("logfile");

	mListener.ListenPort(inport);

	PRINT_LOG << "Config : " << configfile << "\n";
	PRINT_LOG << "Log : " << logfile <<  "\n";
	PRINT_LOG << "PID : " << pidfile <<  "\n";
	PRINT_LOG << "IN Port: " << inport <<  "\n";
	PRINT_LOG << "Ready." <<  "\n";

	Log::SetLogFile(logfile);
}

int Responder::Run()
{
	// LISTEN
	while (true)
	{
		try
		{
			Message mes = mListener.WaitForMessage();

			PRINT_LOG << "Phone: " << mes.GetPhone() << "  Response: " << mes.GetText() << "\n";
		}
		catch (MyException& e)
		{
			Log::Write(e);
		}
	}


	mListener.StopListen();

	return 0;
}



int main(int argc, char** argv)
{
	try
	{
		string pidfile = "/var/run/Responder.pid";
		string configfile = "/etc/config.conf";

		for (int i = 0; i < argc; i++)
		{
			string arg = argv[i];

			if (arg == "--configfile" && i+1 < argc)
			{
				configfile = argv[i+1];
			}
			if (arg == "--pidfile" && i+1 < argc)
			{
				pidfile = argv[i+1];
			}
		}

		// Read config
		Responder Responder(pidfile, configfile);

		// daemon loop
		return Responder.Daemonize();
	}
	catch (MyException& e)
	{
		Log::Write(e);
		return -1;
	}
}
