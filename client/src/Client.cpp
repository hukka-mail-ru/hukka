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
bool Client::connectToHost(const QNetworkProxy& proxy, const QString& hostName, quint16 port)
{
        if(mStatus == CLI_ONLINE) {
                qDebug() << "Connection to host" << hostName << ":" << port << "has been already established";   
                return true;
        }
        
//	mSocket.setProxy(proxy);
	mSocket.connectToHost (hostName, port);

        // wait for establishing connection
        qDebug() << "\nProxy: " << mSocket.proxy().hostName() << ":" << mSocket.proxy().port() << " type=" << mSocket.proxy().type();
        qDebug() << "Host: " << hostName << ":" << port;

        qDebug() << "waiting... ";
        if(!mSocket.waitForConnected(WAIT_CONNECT_TIMEOUT*1000)) {                
                qDebug() << "Connection failed!";  
                mStatus = CLI_OFFLINE;         
                return false;
        }

        qDebug() << "Connected! state = " << mSocket.state();        
        mStatus = CLI_ONLINE;         
        return true;
}

// ====================================================================================================
bool Client::disconnectFromHost() 
{ 
        if(mStatus == CLI_OFFLINE) {
                qDebug() << "Client has already disconnected";   
                return true;
        }

        mSocket.disconnectFromHost();

        qDebug() << "waiting... ";
        if(mSocket.state() != QAbstractSocket::UnconnectedState &&
          !mSocket.waitForDisconnected(WAIT_CONNECT_TIMEOUT*1000)) {                
                qDebug() << "Disconnection failed!";  
                mStatus = CLI_OFFLINE;         
                return false;
        }

        qDebug() << "Disconnected! state = " << mSocket.state();        
        mStatus = CLI_OFFLINE;         
        return true; 
}

// ====================================================================================================
void Client::onConnected()
{
        qDebug() << "SIGNAL connected";
}

// ====================================================================================================
void Client::onError()
{
        qDebug() << "SIGNAL error";
}

// ====================================================================================================
ClientStatus Client::status() { return mStatus; }

LogStatus Client::login(const QString& username, const QString& passwd) { return LOG_OK; }
LogStatus Client::logout(const QString& username) { return LOG_OK; }
