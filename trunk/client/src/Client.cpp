#include "Client.h"
#include <QNetworkProxyQuery>
#include <QtEndian>
#include <deferror.h>
#include <defserver.h>

using namespace std;

#define WAIT_CONNECT_TIMEOUT 3 // seconds
#define WAIT_RESPONSE_TIMEOUT 3 // seconds

#define PROTOCOL_SIGNATURE		'Z'
#define PROTOCOL_VERSION                2
#define CMD_LOGIN                       1


// ====================================================================================================

Client::Client(): mStatus(CLI_OFFLINE) 
{
//        int metaType=qRegisterMetaType<QAbstractSocket::SocketError>("QAbstractSocket::SocketError"); 

        QObject::connect(&mSocket, SIGNAL(connected()), this, SLOT(onConnected()));
        QObject::connect(&mSocket, SIGNAL(error(QAbstractSocket::SocketError)), this, SLOT(onError()));
        //connect(service,SIGNAL(readyRead()),SLOT(parse()));
}

// ====================================================================================================
char Client::getCRC(const QByteArray& data)
{
        char crc = 0;
        for(int i = 0; i < data.size(); i++) {
                crc ^= data[i];
        }

        return crc;
}

// ====================================================================================================
bool Client::connectToHost(const QNetworkProxy& proxy, const QString& hostName, quint16 port)
{
        if(mSocket.state() == QAbstractSocket::ConnectedState) {
                qDebug() << "Connection to server" << hostName << "has been already established";   
                return true;
        }
        
//	mSocket.setProxy(proxy);
	mSocket.connectToHost (hostName, port);

        // wait for establishing connection
        qDebug() << "\nProxy: " << mSocket.proxy().hostName() << ":" << mSocket.proxy().port() << " type=" << mSocket.proxy().type();
        qDebug() << "Server: " << hostName << ":" << port;

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
LogStatus Client::login(const QString& username, const QString& passwd) 
{ 
        if(mSocket.state() != QAbstractSocket::ConnectedState) {
                qDebug() << "Can't login. No connection to server" << mSocket.peerName();   
                return LOG_ERROR;
        }

	// send LOGIN command
        QByteArray data = username.toAscii() + '0' + passwd.toAscii();
        if(!sendCmd(CMD_LOGIN, data)) {
                qDebug() << "Can't login. Can't sent LOGIN command to server" << mSocket.peerName();   
                return LOG_ERROR;
        }
        
	// ger server reply
        if(!mSocket.waitForReadyRead(WAIT_RESPONSE_TIMEOUT*1000)) {
                qDebug() << "Can't login. Server"<< mSocket.peerName() << "does't respond to LOGIN command.";   
                return LOG_ERROR;
        }

	QByteArray buf = mSocket.readAll();
	qDebug() << "LOGIN: Server replied"<< buf.size() << "bytes";   

        MessageHeader* header = (MessageHeader*)buf.data();
	
	if(header->sign != PROTOCOL_SIGNATURE) {
                qDebug() << "Can't login. Server"<< mSocket.peerName() << "uses wrong protocol";   
                return LOG_ERROR;
        }

	if(header->version != PROTOCOL_VERSION) {
                qDebug() << "Can't login. Server"<< mSocket.peerName() << "uses wrong protocol version" << (int)header->version; 
                return LOG_ERROR;
        }

	if(header->cmd != NOERR) {
                qDebug() << "Can't login. Server"<< mSocket.peerName() << "returns error" << (int)header->cmd; 
                // TODO return different errors, depending on reply->cmd
                return LOG_ERROR;
        }
	
        quint32 size = qToLittleEndian(header->size);

        QByteArray infPart((char*)header->version, size - 1);
        if(buf[buf.size() - 1] != getCRC(infPart)) {
                qDebug() << "Can't login. Server"<< mSocket.peerName() << "response has bad CRC"; 
                return LOG_ERROR;
        }

/*
	for(int i=0; i<reply.size(); i++) {
		char c = reply[i];
		qDebug() << (int)c;
	}
*/
        return LOG_OK;
}


// ====================================================================================================
// Arrange a packet and write it to Socket
bool Client::sendCmd(char command, const QByteArray& data)
{
        char crc = 0;

        MessageHeader header;
        header.sign    = PROTOCOL_SIGNATURE;
        header.size    = qToBigEndian(sizeof(header.size) + sizeof(header.address) + data.length() + sizeof(crc));
        header.version = PROTOCOL_VERSION;
        header.address = qToBigEndian(SRV);
        
        QByteArray infPart;
        infPart += header.version;
        infPart += header.address;
        infPart += command;
        infPart += data;

        QByteArray message((char*)&header, sizeof(header));
        message += command;
        message += data;
        message += getCRC(infPart);

        qDebug() << "Sending command with id" << (int)command;  
        qint64 bytes = mSocket.write(message);
        if(bytes == -1) {
                qDebug() << "Can't send command with id" << (int)command;   
                return false;
        }

        Q_ASSERT(bytes == message.size());

	qDebug() << "OK" << bytes << "bytes sent";   

        return true;
}



// ====================================================================================================
ClientStatus Client::status() 
{ 
        if(mSocket.state() == QAbstractSocket::ConnectedState)
                return CLI_ONLINE;

        return CLI_OFFLINE; 
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







