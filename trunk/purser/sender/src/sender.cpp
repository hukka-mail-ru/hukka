//============================================================================
// Name        : sender.cpp
// Author      : 
// Version     :
// Copyright   : Your copyright notice
// Description : Hello World in C++, Ansi-style
//============================================================================


#include <stdlib.h>
#include <string>

#include "Socket.h"
#include "Log.h"
#include "MyException.h"

using namespace std;


int main(int argc, char** argv)
{
	try
	{
		string logfile = "/var/log/sender.log";
		string phone = "default";
		string text = "default";

		PRINT_LOG << "Hello !\n";

		// READ COMMAND LINE
		int port = 1234;

		for (int i = 0; i < argc; i++)
		{

			string arg = argv[i];

			PRINT_LOG << "arg " <<  arg << " \n";

			if (arg == "--port" && i+1 < argc)
			{
				PRINT_LOG << "--port " << " \n";
				port = atoi(argv[i+1]);
			}
			if (arg == "--phone" && i+1 < argc)
			{
				PRINT_LOG << "--phone " << " \n";
				phone = argv[i+1];
			}
			if (arg == "--text" && i+1 < argc)
			{
				PRINT_LOG << "--text " << " \n";
				text = argv[i+1];
			}
		}

		Log::SetLogFile("/home/hukka/devel/purser/log/sender.log");

		PRINT_LOG << "Port: " << port << "\n";
		PRINT_LOG << "Phone: " << phone << "\n";
		PRINT_LOG << "Text: " << text << "\n";
		PRINT_LOG << "Ready.\n";

		//for(int i=0; i<10; i++)
		//{

			Socket socket;
			socket.ConnectToHost("localhost", port);


			Message mes;
			mes.SetPhone(phone);
			mes.SetText(text);

			socket.SendMessage(mes);

		//	socket.DisconnectFromHost();
		//}
	}
	catch (MyException& e)
	{
		PRINT_EX(e);
	}
}
