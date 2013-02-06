//============================================================================
// Name        : sender.cpp
// Author      : 
// Version     :
// Copyright   : Your copyright notice
// Description : Hello World in C++, Ansi-style
//============================================================================


#include <stdlib.h>

#include "Socket.h"
#include "Log.h"
#include "MyException.h"

using namespace std;


int main(int argc, char** argv)
{
	try
	{
		string logfile = "/var/log/sender.log";

		PRINT_LOG << "Hello!\n";

		// READ COMMAND LINE
		int port = 1234;

		for (int i = 0; i < argc; i++)
		{
			string arg = argv[i];

			if (arg == "--port" && i+1 < argc)
			{
				port = atoi(argv[i+1]);
			}
		}

		PRINT_LOG << "Port: " << port << "\n";
		PRINT_LOG << "Ready.\n";


		Socket socket;
		socket.ConnectToHost("localhost", port);


		Message mes;
		mes.SetPhone("+79111112233867867");
		mes.SetText("Hello from sender");

		socket.SendMessage(mes);
	}
	catch (MyException& e)
	{
		Log::Write(e);
	}



}
