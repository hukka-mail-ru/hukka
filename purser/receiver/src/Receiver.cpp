//============================================================================
// Name        : daemon.cpp
// Author      : 
// Version     :
// Copyright   : Your copyright notice
// Description : Hello World in C++, Ansi-style
//============================================================================


#include "Receiver.h"
#include "MyException.h"
#include "Config.h"
#include "Log.h"

using namespace std;

Receiver::Receiver(const std::string& pidfile):
	Daemon(pidfile)
{
	int inport = atoi(Config::GetConfigValue("inport").c_str());
	int outport = atoi(Config::GetConfigValue("outport").c_str());
	string outhost = Config::GetConfigValue("outhost");
	string logfile = Config::GetConfigValue("logfile");

	mListener.ListenPort(inport);

	mSpeaker.SetHost(outhost);
	mSpeaker.SetPort(outport);

	PRINT_LOG << "Log : " << logfile <<  "\n";
	PRINT_LOG << "PID : " << pidfile <<  "\n";
	PRINT_LOG << "IN Port: " << inport <<  "\n";
	PRINT_LOG << "OUT Port: " << outport <<  "\n";
	PRINT_LOG << "Ready." <<  "\n";

	Log::SetLogFile(logfile);
}



void Receiver::Parse(const string& initial, string& parsed, int& start)
{
	unsigned found = initial.find('|', start);
	if (found != std::string::npos)
	{
		parsed = initial.substr(start, found - start);
		start = found + 1;
	}

}

int Receiver::Run()
{
	PRINT_LOG << "Started" << "\n";

	// LISTEN
	while (true)
	{
		try
		{
			Message mes = mListener.WaitForMessage();

			Message reply;
			reply.SetPhone(mes.GetPhone());

			string name;
			string flight;
			string date;
			string purser;
			int start = 0;
			Parse(mes.GetText(), name, start);
			Parse(mes.GetText(), flight, start);
			Parse(mes.GetText(), date, start);
			Parse(mes.GetText(), purser, start);

			date = Base::GetTime( atof(date.c_str()) );

			std::stringstream text;
			text << "Hi, " << name << "! Your flight #" << flight << " on " << date << " confirmed. Purser: " << purser;
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
		Config::ReadConfigFile(configfile);

		Receiver receiver(pidfile);

		// daemon loop
		return receiver.Daemonize();
	}
	catch (MyException& e)
	{
		PRINT_EX(e);
		return -1;
	}
}
