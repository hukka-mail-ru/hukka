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
        if(mSocket.state() == QAbstractSocket::ConnectedState) {
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
                return false;
        }

        qDebug() << "Connected! state = " << mSocket.state();            
        return true;
}

// ====================================================================================================
bool Client::disconnectFromHost() 
{ 
        if(mSocket.state() == QAbstractSocket::UnconnectedState) {
                qDebug() << "Client has already disconnected";   
                return true;
        }

        mSocket.disconnectFromHost();

        qDebug() << "waiting... ";
        if(mSocket.state() != QAbstractSocket::UnconnectedState &&
          !mSocket.waitForDisconnected(WAIT_CONNECT_TIMEOUT*1000)) {                
                qDebug() << "Disconnection failed!";     
                return false;
        }

        qDebug() << "Disconnected! state = " << mSocket.state();               
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
ClientStatus Client::status() 
{ 
        if(mSocket.state() == QAbstractSocket::ConnectedState)
                return CLI_ONLINE;

        return CLI_OFFLINE; 
}

// ====================================================================================================
LogStatus Client::login(const QString& username, const QString& passwd) 
{ 
        if(mSocket.state() != QAbstractSocket::ConnectedState) {
                qDebug() << "Can't login. No connection to host" << mSocket.peerName() << ":" << mSocket.peerPort();   
                return LOG_ERROR;
        }

        QByteArray data = username.toAscii() + '0' + passwd.toAscii();
        if(!sendCmd(CMD_LOGIN, data)) {
                qDebug() << "Can't login. Host does't accept LOGIN command:" << mSocket.peerName() << ":" << mSocket.peerPort();   
                return LOG_ERROR;
        }

        
        // TODO check server response ...

        return LOG_OK;
}

// ====================================================================================================
LogStatus Client::logout(const QString& username) 
{ 
        if(mSocket.state() != QAbstractSocket::ConnectedState) {
                qDebug() << "Can't logout. No connection to host" << mSocket.peerName() << ":" << mSocket.peerPort();   
                return LOG_ERROR;
        }

        return LOG_OK; 
}

// ====================================================================================================
char getCRC(const QByteArray& infPart)
{
        char crc = 0;
        for(int i = 0; i < infPart.size(); i++) {
                crc ^= infPart[i];
        }

        return crc;
}

// ====================================================================================================
#define SIZE_FIELD_VERSION      sizeof(quint8)
#define SIZE_FIELD_ADDRESS      sizeof(quint32)
#define SIZE_FIELD_CRC          sizeof(quint8)

#define PROTOCOL_CURRENT_VERSION        2

bool Client::sendCmd(Command command, const QByteArray& data)
{
        char size[4];
        qsnprintf (size, 4, "%04d", SIZE_FIELD_VERSION + SIZE_FIELD_ADDRESS + data.length() + SIZE_FIELD_CRC);

        char version = PROTOCOL_CURRENT_VERSION;

//        char address[4];
//        qsnprintf (address, 4, "%04d", 0); // ???

        char cmd = 0;
        switch(command) {
                case CMD_LOGIN:         cmd = 1; break;
                case CMD_MY_DISCONNECT: cmd = 2; break;
                default: break;
        };

        QByteArray message = cmd + data;

        QByteArray infPart;
        infPart += version;
//        infPart += address;
        infPart += message;

        char crc = getCRC(infPart);

        QByteArray packet;
        packet += 'Z';
        packet += size;
        packet += infPart;
        packet += crc;

        qDebug() << "Sending command with id" << (int)cmd;  
        qint64 bytes = mSocket.write(packet);
        if(bytes == -1) {
                qDebug() << "Can't send command with id" << (int)cmd;   
                return false;
        }

        Q_ASSERT(bytes == packet.size());
//        waitForBytesWritten


        return true;
}







