#include "Client.h"
#include <QNetworkProxyQuery>

using namespace std;

#define WAIT_CONNECT_TIMEOUT 10 // seconds

// ====================================================================================================

Client::Client(): mStatus(CLI_OFFLINE) 
{
//        int metaType=qRegisterMetaType<QAbstractSocket::SocketError>("QAbstractSocket::SocketError"); 

        QObject::connect(&mSocket, SIGNAL(connected()), this, SLOT(onConnected()));
        QObject::connect(&mSocket, SIGNAL(error(QAbstractSocket::SocketError)), this, SLOT(onError()));
//connect(service,SIGNAL(readyRead()),SLOT(parse()));
}

// ====================================================================================================
ClientStatus Client::connectToHost(const QNetworkProxy& proxy, const QString& hostName, quint16 port)
{


//	mSocket.setProxy(proxy);
	mSocket.connectToHost (hostName, port);

/*
     socketDevice.setProxy(QNetworkProxy(QNetworkProxy::HttpProxy, QtNetworkSettings::serverName(), 3128)); 
     QVERIFY(!socketDevice.connectToHost(QtNetworkSettings::serverIP(), 143));
     QVERIFY(socketDevice.state() == QAbstractSocket::ConnectingState);
     QVERIFY(socketDevice.waitForWrite());
*/

        // wait for establishing connection
        qDebug() << "\nProxy: " << mSocket.proxy().hostName() << ":" << mSocket.proxy().port() << " type=" << mSocket.proxy().type();
        qDebug() << "Host: " << hostName << ":" << port;

        qDebug() << "waiting... ";
        if(!mSocket.waitForConnected(WAIT_CONNECT_TIMEOUT*1000)) {                
                qDebug() << "Timeout reached!";                
                return CLI_OFFLINE;
        }

        qDebug() << "Connected! state = " << mSocket.state();
        
        return CLI_ONLINE;
}


// ====================================================================================================
void Client::onConnected()
{
        qDebug() << "connected";
}

// ====================================================================================================
void Client::onError()
{
        qDebug() << "error";
}

// ====================================================================================================
ClientStatus Client::disconnectFromHost() { return CLI_OFFLINE; }
ClientStatus Client::status() { return mStatus; }

LogStatus Client::login(const QString& username, const QString& passwd) { return LOG_OK; }
LogStatus Client::logout(const QString& username) { return LOG_OK; }
