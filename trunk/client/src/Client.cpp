#include "Client.h"
#include <QNetworkProxyQuery>

using namespace std;

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

                        QNetworkProxyQuery query(hostName, port);
                        QList<QNetworkProxy> proxies = QNetworkProxyFactory::proxyForQuery(query);
                        mSocket.setProxy(QNetworkProxy::NoProxy);
                        foreach (const QNetworkProxy &p, proxies)
                        {
                                if((p.capabilities() & QNetworkProxy::TunnelingCapability) == 0)
                                        continue;
                                mSocket.setProxy(p);
                                break;
                        }


//	mSocket.setProxy(proxy);
	mSocket.connectToHost (hostName, port);

/*
     socketDevice.setProxy(QNetworkProxy(QNetworkProxy::HttpProxy, QtNetworkSettings::serverName(), 3128)); 
     QVERIFY(!socketDevice.connectToHost(QtNetworkSettings::serverIP(), 143));
     QVERIFY(socketDevice.state() == QAbstractSocket::ConnectingState);
     QVERIFY(socketDevice.waitForWrite());
*/

        // wait for establishing connection
        qDebug("\n\n");
        qDebug() << mSocket.proxy().hostName();
        qDebug() << mSocket.proxy().port();
        qDebug() << mSocket.proxy().type();
        qDebug("\n\n");

        qDebug() << hostName;
        qDebug() << port;
        qDebug("\n\n");
        for(;;)
        {                
                qDebug() << "waiting... " << mSocket.state();
                sleep(10);
        }

        qDebug() << "Client::connect finished";
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
