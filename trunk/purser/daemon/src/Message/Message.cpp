
#include "Message.h"
#include "Log.h"

using namespace std;

Message Message::Parse(const char* buf)
{

	Message mes;
	mes.mSignature = buf[0];
	mes.mPhoneLen = buf[1];
	mes.mTextLen = buf[2];
	mes.mCrc = buf[3];

	string str(buf);

	Log::Write("Received: " + str);
	Log::Write("MESSAGE_SIZE: " + to_string(MESSAGE_SIZE));

	Log::Write(string("mes.signature: ") + mes.mSignature);
	Log::Write(string("mes.phone_len: ") + to_string(mes.mPhoneLen));
	Log::Write(string("mes.text_len: ") + to_string(mes.mTextLen));
	Log::Write(string("mes.crc: ") + to_string(mes.mCrc));

	return mes;
}

const char* Message::Serialize()
{

}
