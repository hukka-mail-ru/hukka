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


int Receiver::Run()
{
	// LISTEN
	while (true)
	{
		try
		{
			Socket insocket = GetListeningSocket();
			insocket.Open();

			// Get new message
			Message mes = insocket.ReceiveMessage();

			insocket.Close();

			// send response
			Socket outsocket;
			outsocket.ConnectToHost("localhost", mOutport);

			Message reply;
			reply.SetPhone(mes.GetPhone());
			reply.SetText("This is a reply");

			outsocket.SendMessage(reply);

		}
		catch (MyException& e)
		{
			Log::Write(e);
		}
	}

	StopListen();

	return 0;
}


int main(int argc, char** argv)
{
	try
	{
		string logfile = "/var/log/receiver.log";
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
		int inport = atoi(receiver.GetConfigValue("inport").c_str());
		int outport = atoi(receiver.GetConfigValue("outport").c_str());
		logfile = receiver.GetConfigValue("logfile");

		receiver.SetOutport(outport);
		receiver.ListenPort(inport);

		PRINT_LOG << "Config : " << configfile << "\n";
		PRINT_LOG << "Log : " << logfile <<  "\n";
		PRINT_LOG << "PID : " << pidfile <<  "\n";
		PRINT_LOG << "IN Port: " << inport <<  "\n";
		PRINT_LOG << "OUT Port: " << outport <<  "\n";
		PRINT_LOG << "Ready." <<  "\n";

		Log::SetLogFile(logfile);

		// daemon loop
		return receiver.Daemonize();
	}
	catch (MyException& e)
	{
		Log::Write(e);
		return -1;
	}
}
