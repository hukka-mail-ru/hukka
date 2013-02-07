/*
 * Listener.cpp
 *
 *  Created on: 7 Feb 2013
 *      Author: hukka
 */

#include "Listener.h"
#include "Log.h"

Listener::Listener() {
	// TODO Auto-generated constructor stub

}

Listener::~Listener() {
	// TODO Auto-generated destructor stub
}

// STOP LISTENING
void Listener::StopListen()
{
	mSocket.StopListen();
}

void Listener::ListenPort(int port)
{
	mSocket.Listen(port);
	PRINT_LOG << "Listen: " << port << "\n";
}
