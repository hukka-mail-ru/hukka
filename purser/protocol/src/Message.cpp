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

	if(signature != PROTOCOL_SIGNATURE)
	{
		throw ExceptionProtocolError(PRINT_WHERE, "Wrong protocol signature");
	}

	mPhoneLen = buf[1];
	mTextLen = buf[2];
	char crc = buf[3];

	for(unsigned i = PHONE_OFFSET; i < PHONE_OFFSET + mPhoneLen; i++)
	{
	   mPhone += buf[i];
	}

	for(unsigned i = TEXT_OFFSET; i < TEXT_OFFSET + mTextLen; i++)
	{
	   mText += buf[i];
	}

	if(crc != GetCRC(mText))
	{
		throw ExceptionProtocolError(PRINT_WHERE, "Wrong CRC");
	}

	string str(buf);
}

string Message::Serialize() const
{
	string str;

	char crc = GetCRC(mText);

	str += PROTOCOL_SIGNATURE;
    str += char(mPhone.length());
	str += char(mText.length());
	str += crc;
	str += mPhone;
	str += string(MAX_PHONE_LEN - mPhone.length(), '\0');
	str += mText;
	str += string(MAX_TEXT_LEN - mText.length(), '\0');

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
