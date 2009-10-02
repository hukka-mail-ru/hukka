#include "Client.h"
#include "Exception.h"
#include <QNetworkProxyQuery>
#include <QtEndian>


using namespace std;

#define WAIT_CONNECT_TIMEOUT 3 // seconds
#define WAIT_RESPONSE_TIMEOUT 3 // seconds

#define PROTOCOL_SIGNATURE		'Z'
#define PROTOCOL_VERSION                2
#define CMD_LOGIN                       1

// ====================================================================================================
char getCRC(const QByteArray& data)
{
        char crc = 0;
        for(int i = 0; i < data.size(); i++) {
                crc ^= data[i];
        }

        return crc;
}

        
// ====================================================================================================

Client::Client(): mStatus(CLI_OFFLINE) 
{
//        int metaType=qRegisterMetaType<QAbstractSocket::SocketError>("QAbstractSocket::SocketError"); 

        QObject::connect(&mSocket, SIGNAL(connected()), this, SLOT(onConnected()));
        QObject::connect(&mSocket, SIGNAL(error(QAbstractSocket::SocketError)), this, SLOT(onError()));
        //connect(service,SIGNAL(readyRead()),SLOT(parse()));
}

// ====================================================================================================
void Client::connectToHost(const QNetworkProxy& proxy, const QString& hostName, quint16 port)
{
        if(mSocket.state() == QAbstractSocket::ConnectedState) {
                qDebug() << "Connection to server" << hostName << "has been already established";   
                return;
        }
        
//	mSocket.setProxy(proxy);
	mSocket.connectToHost (hostName, port);

        // wait for establishing connection
        qDebug() << "Proxy: " << mSocket.proxy().hostName() << ":" << mSocket.proxy().port() << " type=" << mSocket.proxy().type();
        qDebug() << "Server: " << hostName << ":" << port;

        qDebug() << "waiting... ";
        if(!mSocket.waitForConnected(WAIT_CONNECT_TIMEOUT*1000)) {                
                THROW_EXCEPTION("Connection failed! Can't connect to server: " + hostName);
        }

        qDebug() << "Connected! state = " << mSocket.state();            
}

// ====================================================================================================
void Client::disconnectFromHost() 
{ 
        if(mSocket.state() == QAbstractSocket::UnconnectedState) {
                qDebug() << "Client has already disconnected";   
                return;
        }

        mSocket.disconnectFromHost();

        qDebug() << "waiting... ";
        if(mSocket.state() != QAbstractSocket::UnconnectedState &&
          !mSocket.waitForDisconnected(WAIT_CONNECT_TIMEOUT*1000)) {  
                THROW_EXCEPTION("Disconnection failed! Can't disconnect from server: " + mSocket.peerName());              
        }

        qDebug() << "Disconnected! state = " << mSocket.state();               
}


// ====================================================================================================
void Client::login(const QString& username, const QString& passwd) 
{ 
        #define LOGIN_ERROR_HEAD     "Can't login to server " + mSocket.peerName() + ". "

        if(mSocket.state() != QAbstractSocket::ConnectedState) {
                THROW_EXCEPTION(LOGIN_ERROR_HEAD + "Connection has been lost");   
        }

	// send LOGIN command
        QByteArray data = (username + QChar(0) + passwd).toAscii();
        sendCmd(CMD_LOGIN, data);
        
	// get server reply
        if(!mSocket.waitForReadyRead(WAIT_RESPONSE_TIMEOUT*1000)) {
                THROW_EXCEPTION(LOGIN_ERROR_HEAD + "Server does't respond to LOGIN command.");   
        }

	QByteArray buf = mSocket.readAll();
	qDebug() << "LOGIN: Server replied"<< buf.size() << "bytes";   
/*
	for(int i=0; i<buf.size(); i++) {
		char c = buf[i];
		qDebug() << (int)c;
	}

	qDebug() << "sizeof()" << sizeof(h);
	qDebug() << "header->sign" << (int)header->sign;
	qDebug() << "header->size" << qToLittleEndian(header->size);
	qDebug() << "header->version" << (int)header->version;
	qDebug() << "header->address" << qToLittleEndian(header->address);
	qDebug() << "header->cmd" << header->cmd;
*/	
        MessageHeader* header = (MessageHeader*)buf.data();

	if(header->sign != PROTOCOL_SIGNATURE) {
                THROW_EXCEPTION(LOGIN_ERROR_HEAD + "Server uses wrong protocol ");   
        }

	if(header->version != PROTOCOL_VERSION) {
                THROW_EXCEPTION(LOGIN_ERROR_HEAD + "Server uses wrong protocol version: " + (int)header->version); 
        }

	if(qToLittleEndian(header->address) != SRV) {
                THROW_EXCEPTION(LOGIN_ERROR_HEAD + "Server uses wrong service: " + qToLittleEndian(header->address)); 
        }

        switch(header->cmd) {
                case NOERR:          break;
                case ERRUSERONLINE:  qDebug() << "User"<< username << "is already online"; return; break;
                case ERRBADLOGIN:    THROW_EXCEPTION(LOGIN_ERROR_HEAD + "Incorrect user name: " + username); break;
                case ERRBADPASS:     THROW_EXCEPTION(LOGIN_ERROR_HEAD + "Incorrect password for user: " + username); break;
                default:             THROW_EXCEPTION(LOGIN_ERROR_HEAD + "Internal server error " + (int)header->cmd); break;
        } 
	
        quint32 size = qToLittleEndian(header->size);

        QByteArray infPart((char*)header->version, size - 1);
        if(buf[buf.size() - 1] != getCRC(infPart)) {
                THROW_EXCEPTION(LOGIN_ERROR_HEAD + "Server response has bad CRC"); 
        }
}


// ====================================================================================================
// Arrange a packet and write it to Socket
void Client::sendCmd(char command, const QByteArray& data)
{
        char crc = 0;

        MessageHeader header;
        header.sign    = PROTOCOL_SIGNATURE;
        header.version = PROTOCOL_VERSION;
        header.address = SRV;
	header.cmd = command;
        
        QByteArray infPart;
        infPart += header.version;
        infPart += QByteArray((char*)&header.address, sizeof(header.address));
        infPart += command;
        infPart += data;

        header.size = infPart.length() + sizeof(crc);

        QByteArray message((char*)&header, sizeof(header));
        message += data;
        message += getCRC(infPart);

for(int i=0; i<message.size(); i++) {
	char c = message[i];
	qDebug() << (int)c;
}

        qDebug() << "Sending command with id" << (int)command;  
        qint64 bytes = mSocket.write(message);
        if(bytes == -1) {
                THROW_EXCEPTION("Can't send command  to server " + mSocket.peerName() + ". Command ID: " + (int)command);   
        }

        Q_ASSERT(bytes == message.size());

	qDebug() << "OK" << bytes << "bytes sent";   
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







