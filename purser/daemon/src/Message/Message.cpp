#include <stdio.h>

#include "Message.h"
#include "Log.h"

using namespace std;

Message Message::Parse(const char* buf)
{

	Message mes;
	char signature = buf[0];
	mes.mPhoneLen = buf[1];
	mes.mTextLen = buf[2];
	char crc = buf[3];

	string str(buf);

	Log::Write("Received: " + str);
	Log::Write("MESSAGE_SIZE: " + to_string(MESSAGE_SIZE));

	Log::Write(string("mes.signature: ") + signature);
	Log::Write(string("mes.phone_len: ") + to_string(mes.mPhoneLen));
	Log::Write(string("mes.text_len: ") + to_string(mes.mTextLen));
	Log::Write(string("mes.crc: ") + to_string(crc));

	return mes;
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
	str += string(MAX_TEXT_LEN - mPhone.length(), '\0');

	Log::Write("==============");
	Log::Write("Phone: " + mPhone + "; Len " + to_string(mPhone.length()));
	Log::Write("Text: " + mText + "; Len " + to_string(mText.length()));
	Log::Write("CRC: " + to_string(toascii(crc)));
	Log::WriteBytes(str);
	Log::Write("Len: " + to_string(str.length()));

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
