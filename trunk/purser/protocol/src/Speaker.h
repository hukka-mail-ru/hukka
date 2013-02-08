/*
 * Speaker.h
 *
 *  Created on: 8 Feb 2013
 *      Author: hukka
 */

#ifndef SPEAKER_H_
#define SPEAKER_H_

#include "Base.h"
#include "Message.h"
#include <string>

class Speaker {
public:
	Speaker();
	virtual ~Speaker();

	SETTER_(std::string, Host);
	SETTER_(unsigned, Port);

	void Speak(const Message& message);

private:
	std::string mHost;
	unsigned mPort;
};

#endif /* SPEAKER_H_ */
