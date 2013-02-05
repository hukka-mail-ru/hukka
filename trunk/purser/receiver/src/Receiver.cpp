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
			mSocket.Open();

			// Get new message
			Message mes = mSocket.ReceiveMessage();

			Message reply;
			reply.SetPhone("+79119089209");
			reply.SetText("This is a normal reply");

			mSocket.SendMessage(reply);

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

void Receiver::ListenPort(int port)
{
	mSocket.Listen(port);
	PRINT_LOG << "Listen: " << port << "\n";
}


