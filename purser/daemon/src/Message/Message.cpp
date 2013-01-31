
#include "Message.h"
//#include "Log.h"

using namespace std;

Message Message::Parse(const char* buf)
{

	Message mes;
	mes.mSignature = buf[0];
	mes.mPhoneLen = buf[1];
	mes.mTextLen = buf[2];
	mes.mCrc = buf[3];

/*
	Log::Write("Received: " + str);
	Log::Write("MESSAGE_SIZE: " + to_string(MESSAGE_SIZE));

	string sign = mes.signature;
	unsigned tel_size =

	Log::Write(string("mes.signature: ") + mes.signature);
	Log::Write(string("mes.phone_len: ") + mes.phone_len);
	Log::Write(string("mes.text_len: ") + mes.text_len);
	Log::Write(string("mes.crc: ") + mes.crc);

*/
	return mes;
}

const char* Message::Serialize()
{

}
