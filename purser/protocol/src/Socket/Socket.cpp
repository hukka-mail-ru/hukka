/*
 * Client.cpp
 *
 *  Created on: 31 Jan 2013
 *      Author: hukka
 */

#include <sys/socket.h>

#include <string>

#include "Socket.h"

using namespace std;

Message Socket::ReceiveMessage(int client)
{
	char buf[MESSAGE_SIZE] = {'0'};
	recv(client, buf, MESSAGE_SIZE, MSG_WAITALL);

	Message mes = Message::Parse(buf);

	return mes;
}


void Socket::SendMessage(int client, const Message& mes)
{
	string str = mes.Serialize();

	send(client, str.c_str(), str.length(), 0);
}
