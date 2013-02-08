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

	SETTER_(unsigned, Port);

	void Speak(const std::string& host, const Message& message);

private:
	unsigned mPort;
};

#endif /* SPEAKER_H_ */
