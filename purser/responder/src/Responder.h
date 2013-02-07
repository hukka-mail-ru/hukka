/*
 * Responder.h
 *
 *  Created on: 5 Feb 2013
 *      Author: hukka
 */

#ifndef Responder_H_
#define Responder_H_

#include "Socket.h"
#include "Daemon.h"

class Responder: public Daemon
{
public:
	Responder(const std::string& pidfile, const std::string& configfile):
		Daemon(pidfile, configfile) {}

	virtual int Run();

	void ListenPort(int port);

private:
	Socket mSocket;
};


#endif /* Responder_H_ */
