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



int Responder::Run()
{
	// LISTEN
	while (true)
	{
		try
		{
			mSocket.Open();

			// Get new message
			Message mes = mSocket.ReceiveMessage();

			PRINT_LOG << "Phone: " << mes.GetPhone() << "  Responce: " << mes.GetText() << "\n";

			mSocket.Close();
		}
		catch (MyException& e)
		{
			Log::Write(e);
		}
	}

	// STOP LISTENING
	try
	{
		mSocket.StopListen();
	}
	catch (MyException& e)
	{
		Log::Write(e);
		return -1;
	}

	return 0;
}

void Responder::ListenPort(int port)
{
	mSocket.Listen(port);
	PRINT_LOG << "Listen: " << port << "\n";
}


int main(int argc, char** argv)
{
	try
	{
		string logfile = "/var/log/Responder.log";

		// READ COMMAND LINE
		string pidfile = "/var/run/Responder.pid";
		int inport = 1111;

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
		inport = atoi(Responder.GetConfigValue("inport").c_str());
		logfile = Responder.GetConfigValue("logfile");

		Responder.ListenPort(inport);

		PRINT_LOG << "Config : " << configfile << "\n";
		PRINT_LOG << "Log : " << logfile <<  "\n";
		PRINT_LOG << "PID : " << pidfile <<  "\n";
		PRINT_LOG << "IN Port: " << inport <<  "\n";
		PRINT_LOG << "Ready." <<  "\n";

		Log::SetLogFile(logfile);

		// daemon loop
		return Responder.Daemonize();
	}
	catch (MyException& e)
	{
		Log::Write(e);
		return -1;
	}
}
