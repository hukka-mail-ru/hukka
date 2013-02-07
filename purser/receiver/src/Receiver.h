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
#include "Listener.h"

class Receiver: public Daemon, public Listener
{
public:
	Receiver(const std::string& pidfile, const std::string& configfile):
		Daemon(pidfile, configfile) {}

	virtual int Run();

	SETTER_(int, Outport);

private:

	int mOutport;
};


#endif /* RECEIVER_H_ */
