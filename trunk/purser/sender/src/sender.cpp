//============================================================================
// Name        : sender.cpp
// Author      : 
// Version     :
// Copyright   : Your copyright notice
// Description : Hello World in C++, Ansi-style
//============================================================================


#include <iostream>
#include <string>

#include <stdlib.h>

#include "Socket.h"
#include "Log.h"

using namespace std;


int main(int argc, char** argv)
{
	cout << "hello" << endl;

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


	Socket socket;
	socket.ConnectToHost("localhost", port);


	Message mes;
	mes.setPhone("+79115361051");
	mes.setText("Hello from home");

	socket.SendMessage(mes);



}
