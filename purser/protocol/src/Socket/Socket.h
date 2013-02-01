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

	void ConnectToHost(const std::string& host, unsigned port);

	void Listen(int port);
	void StopListen() const;

	void OpenAndWaitForConnection();
	void Close() const;

	Message ReceiveMessage() const;
	void SendMessage(const Message& mes) const;



private:

	int mSockfd;
	int mListener;
};

#endif /* CLIENT_H_ */
