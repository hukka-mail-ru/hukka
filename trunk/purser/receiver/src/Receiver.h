/*
 * receiver.h
 *
 *  Created on: 5 Feb 2013
 *      Author: hukka
 */

#ifndef RECEIVER_H_
#define RECEIVER_H_

#include "Socket.h"
#include "Daemon.h"

class Receiver: public Daemon
{
public:
	Receiver(const std::string& pidfile): Daemon(pidfile) {}

	virtual int Run();

	void ListenPort(int port);

private:
	Socket mSocket;
};


#endif /* RECEIVER_H_ */
