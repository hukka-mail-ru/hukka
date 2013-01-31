/*
 * Message.h
 *
 *  Created on: 30 Jan 2013
 *      Author: hukka
 */

#ifndef MESSAGE_H_
#define MESSAGE_H_

struct Message
{
	char signature;
	char phone_len;
	char text_len;
	char crc;
//	char phone[32];
//	char text[220];
};


#endif /* MESSAGE_H_ */
