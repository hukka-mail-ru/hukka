/*
 * receiver.h
 *
 *  Created on: 5 Feb 2013
 *      Author: hukka
 */

#ifndef RECEIVER_H_
#define RECEIVER_H_

#include "Daemon.h"
#include "Listener.h"
#include "Speaker.h"

class Receiver: public Daemon
{
public:
	Receiver(const std::string& pidfile);

	virtual int Run();

private:

	void Parse(const std::string& initial, std::string& parsed, int& start);

    Listener mListener;
    Speaker mSpeaker;
};


#endif /* RECEIVER_H_ */
