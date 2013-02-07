/*
 * Listener.h
 *
 *  Created on: 7 Feb 2013
 *      Author: hukka
 */

#ifndef LISTENER_H_
#define LISTENER_H_

#include "Socket.h"

class Listener {
public:
	Listener();
	virtual ~Listener();

	void ListenPort(int port);

	void StopListen();

	Socket GetListeningSocket() { return mSocket; }

private:
	Socket mSocket;

};

#endif /* LISTENER_H_ */
