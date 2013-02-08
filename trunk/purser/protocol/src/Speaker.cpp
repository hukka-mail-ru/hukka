/*
 * Speaker.cpp
 *
 *  Created on: 8 Feb 2013
 *      Author: hukka
 */

#include "Speaker.h"
#include "Socket.h"
#include "MyException.h"

Speaker::Speaker() {
	// TODO Auto-generated constructor stub

}

Speaker::~Speaker() {
	// TODO Auto-generated destructor stub
}

void Speaker::Speak(const Message& message)
{
	// send response
	Socket socket;
	socket.ConnectToHost(mHost, mPort);

	socket.SendMessage(message);
}
