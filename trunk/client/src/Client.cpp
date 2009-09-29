#include <assert.h>
#include <iostream>
#include "Client.h"

using namespace std;

Client::Client(): mStatus(CLI_OFFLINE) 
{
 //       QObject::connect(&mSocket, SIGNAL(connected()), this, SLOT(onConnected()));
 //       QObject::connect(&mSocket, SIGNAL(error()), this, SLOT(onError()));
}

ClientStatus Client::connectToHost(const QNetworkProxy& proxy, const QString& hostName, quint16 port)
{
	mSocket.setProxy(proxy);
	mSocket.connectToHost (hostName, port);

        // wait for establishing connection
/*
        for(;;)
        {
                // timeout
        }
*/
        cout << "Client::connect finished" << endl;
}


void Client::onConnected()
{
        cout << "connected" << endl;
}


void Client::onError()
{
        cout << "error" << endl;
}


ClientStatus Client::disconnectFromHost() { return CLI_OFFLINE; }
ClientStatus Client::status() { return mStatus; }

LogStatus Client::login(const QString& username, const QString& passwd) { return LOG_OK; }
LogStatus Client::logout(const QString& username) { return LOG_OK; }
