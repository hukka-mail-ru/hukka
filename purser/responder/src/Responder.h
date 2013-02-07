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
#include "Listener.h"

class Responder: public Daemon, public Listener
{
public:
	Responder(const std::string& pidfile, const std::string& configfile):
		Daemon(pidfile, configfile) {}

	virtual int Run();
};


#endif /* Responder_H_ */
