/*
 * Message.h
 *
 *  Created on: 30 Jan 2013
 *      Author: hukka
 */

#ifndef MESSAGE_H_
#define MESSAGE_H_

#include <string>

const unsigned MESSAGE_SIZE = 4;

class Message
{
public:

	static Message Parse(const char* buf);

	const char* Serialize();

	std::string getPhone() { return mPhone; }
	std::string getText() { return mText; }

private:

	char mSignature;
	unsigned mPhoneLen;
	unsigned mTextLen;
	unsigned mCrc;
	std::string mPhone;
	std::string mText;
};




#endif /* MESSAGE_H_ */
