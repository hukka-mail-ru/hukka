//============================================================================
// Name        : daemon.cpp
// Author      : 
// Version     :
// Copyright   : Your copyright notice
// Description : Hello World in C++, Ansi-style
//============================================================================

#include <iostream>

#include "Responder.h"
#include "MyException.h"
#include "Config.h"
#include "Log.h"

using namespace std;

Responder::Responder(const std::string& pidfile):
	Daemon(pidfile)
{
	int inport = atoi(Config::GetConfigValue("inport").c_str());
	string logfile = Config::GetConfigValue("logfile");

	mListener.ListenPort(inport);

	PRINT_LOG << "Log : " << logfile <<  "\n";
	PRINT_LOG << "PID : " << pidfile <<  "\n";
	PRINT_LOG << "IN Port: " << inport <<  "\n";
	PRINT_LOG << "Ready." <<  "\n";

	Log::SetLogFile(logfile);
}

int Responder::Run()
{
	PRINT_LOG << "Started" << "\n";

	string sendsms = Config::GetConfigValue("sendsms");

	// LISTEN
	while (true)
	{
		try
		{
			Message mes = mListener.WaitForMessage();

			string command = sendsms + " " + mes.GetPhone() + " '" + mes.GetText() + "'";

			PRINT_LOG << "Run: " << command << "\n";

			int status = system(command.c_str());
			if(status < 0)
			{
				THROW_EX(MyException()) << "Error sending SMS: " << status;
			}

		}
		catch (MyException& e)
		{
			PRINT_EX(e);
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
		Config::ReadConfigFile(configfile);

		Responder Responder(pidfile);

		// daemon loop
		return Responder.Daemonize();
	}
	catch (MyException& e)
	{
		PRINT_EX(e);
		return -1;
	}
}
