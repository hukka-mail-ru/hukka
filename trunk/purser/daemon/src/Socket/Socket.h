/*
 * Client.h
 *
 *  Created on: 31 Jan 2013
 *      Author: hukka
 */

#ifndef CLIENT_H_
#define CLIENT_H_

#include "Message.h"


class Socket {
public:
	static Message ReceiveMessage(int client);
	static void SendMessage(int client, const Message& mes);
};

#endif /* CLIENT_H_ */
