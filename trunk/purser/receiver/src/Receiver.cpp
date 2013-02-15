//============================================================================
// Name        : daemon.cpp
// Author      : 
// Version     :
// Copyright   : Your copyright notice
// Description : Hello World in C++, Ansi-style
//============================================================================


#include "Receiver.h"
#include "MyException.h"
#include "Log.h"

using namespace std;

Receiver::Receiver(const std::string& pidfile, const std::string& configfile):
	Daemon(pidfile, configfile)
{
	int inport = atoi(GetConfigValue("inport").c_str());
	int outport = atoi(GetConfigValue("outport").c_str());
	string outhost = GetConfigValue("outhost");
	string logfile = GetConfigValue("logfile");

	mListener.ListenPort(inport);

	mSpeaker.SetHost(outhost);
	mSpeaker.SetPort(outport);

	PRINT_LOG << "Config : " << configfile << "\n";
	PRINT_LOG << "Log : " << logfile <<  "\n";
	PRINT_LOG << "PID : " << pidfile <<  "\n";
	PRINT_LOG << "IN Port: " << inport <<  "\n";
	PRINT_LOG << "OUT Port: " << outport <<  "\n";
	PRINT_LOG << "Ready." <<  "\n";

	Log::SetLogFile(logfile);
}


int Receiver::Run()
{
	// LISTEN
	while (true)
	{
		try
		{
			Message mes = mListener.WaitForMessage();

			Message reply;
			reply.SetPhone(mes.GetPhone());

			std::stringstream text;
			text << "You wrote: " << mes.GetText() << " - Automatic answer: ku-ku :)";
			reply.SetText(text.str());

			mSpeaker.Speak(reply);

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
		string pidfile = "/var/run/receiver.pid";
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
		Receiver receiver(pidfile, configfile);

		// daemon loop
		return receiver.Daemonize();
	}
	catch (MyException& e)
	{
		PRINT_EX(e);
		return -1;
	}
}
