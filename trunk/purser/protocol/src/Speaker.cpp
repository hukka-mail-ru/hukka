/*
 * Speaker.cpp
 *
 *  Created on: 8 Feb 2013
 *      Author: hukka
 */

#include "Speaker.h"
#include "Socket.h"

Speaker::Speaker() {
	// TODO Auto-generated constructor stub

}

Speaker::~Speaker() {
	// TODO Auto-generated destructor stub
}

void Speaker::Speak(const std::string& host, const Message& message)
{
	// send response
	Socket socket;
	socket.ConnectToHost(host, mPort);

	socket.SendMessage(message);
}
