#include <stdio.h>

#include "Message.h"
#include "Log.h"
#include "MyException.h"

using namespace std;

Message::Message(const char* buf)
{
	Parse(buf);
}

void Message::Parse(const char* buf)
{
	char signature = buf[0];

	if(signature != ProtocolSignature)
	{
		THROW_EX(ExceptionProtocolError) << "Wrong protocol signature";
	}

	mPhoneLen = buf[1];
	mTextLen = buf[2];
	char crc = buf[3];

	for(unsigned i = PhoneOffset; i < PhoneOffset + mPhoneLen; i++)
	{
	   mPhone += buf[i];
	}

	for(unsigned i = TextOffset; i < TextOffset + mTextLen; i++)
	{
	   mText += buf[i];
	}

	if(crc != GetCRC(mText))
	{
		THROW_EX(ExceptionProtocolError) << "Wrong CRC";
	}

	string str(buf);
}

string Message::Serialize() const
{
	string str;

	char crc = GetCRC(mText);

	str += ProtocolSignature;
    str += char(mPhone.length());
	str += char(mText.length());
	str += crc;
	str += mPhone;
	str += string(MaxPhoneLen - mPhone.length(), '\0');
	str += mText;
	str += string(MaxTextLen - mText.length(), '\0');

	return str;
}


char Message::GetCRC(const string& str) const
{
    char crc = 0;
    for(char c: str)
    {
        crc ^= c;
    }

    return crc;
}
