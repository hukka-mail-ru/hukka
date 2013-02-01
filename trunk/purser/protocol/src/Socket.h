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

	// client
	void ConnectToHost(const std::string& host, unsigned port);

	// server
	void Listen(int port);
	void StopListen() const;

	// server
	void Open();
	void Close() const;

	// both
	Message ReceiveMessage() const;
	void SendMessage(const Message& mes) const;


private:

	int mSockfd;
	int mListener;
	int mPort; // just for logs
};

#endif /* CLIENT_H_ */
