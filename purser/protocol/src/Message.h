/*
 * Message.h
 *
 *  Created on: 30 Jan 2013
 *      Author: hukka
 */

#ifndef MESSAGE_H_
#define MESSAGE_H_

#include <string>


const char ProtocolSignature = 'Z';

const unsigned MaxPhoneLen = 32;
const unsigned MaxTextLen = 220;

const unsigned PhoneOffset = 4;
const unsigned TextOffset = PhoneOffset + MaxPhoneLen;

const unsigned MessageSize = PhoneOffset + MaxPhoneLen + MaxTextLen;


class Message
{
public:

	Message() {}
	Message(const char* buf);

	std::string Serialize() const;

	std::string GetPhone() const { return mPhone; }
	std::string GetText() const { return mText; }

	void SetPhone(const std::string& phone) { mPhone = phone; }
	void SetText(const std::string& text) { mText = text; }

private:

	void Parse(const char* buf);
	char GetCRC(const std::string& str) const;

	unsigned mPhoneLen;
	unsigned mTextLen;
	std::string mPhone;
	std::string mText;
};




#endif /* MESSAGE_H_ */
