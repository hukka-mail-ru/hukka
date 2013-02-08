/*
 * Responder.h
 *
 *  Created on: 5 Feb 2013
 *      Author: hukka
 */

#ifndef Responder_H_
#define Responder_H_

#include "Daemon.h"
#include "Listener.h"

class Responder: public Daemon
{
public:
	Responder(const std::string& pidfile, const std::string& configfile);

	virtual int Run();
private:

    Listener mListener;
};


#endif /* Responder_H_ */
