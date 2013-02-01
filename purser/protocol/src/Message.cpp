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

	for(unsigned i = PHONE_OFFSET; i < PHONE_OFFSET + mes.mPhoneLen; i++)
	{
	   mes.mPhone += buf[i];
	}

	for(unsigned i = TEXT_OFFSET; i < TEXT_OFFSET + mes.mTextLen; i++)
	{
	   mes.mText += buf[i];
	}

	string str(buf);

	Log() << " === INCOMING === \n";
	Log() << "MESSAGE_SIZE: " << MESSAGE_SIZE << "\n";

	Log() << "mes.sign ature: " << signature << "\n";
	Log() << "mes.phone_len: " << mes.mPhoneLen << "\n";
	Log() << "mes.text_len: " << mes.mTextLen << "\n";
	Log() << "mes.crc: " << toascii(crc) << "\n";
	Log() << "mes.phone: " << mes.mPhone << "\n";
	Log() << "mes.text: " << mes.mText << "\n";

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
	str += string(MAX_TEXT_LEN - mText.length(), '\0');

	Log() << "======= OUTGOING  =======" << "\n";
	Log() << "Phone: " << mPhone << "; Len " << mPhone.length() << "\n";
	Log() << "Text: " << mText << "; Len " << mText.length() << "\n";
	Log() << "CRC: " << toascii(crc) << "\n";
	Log::WriteBytes(str);
	Log() << "Len: " + to_string(str.length()) << "\n";

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
