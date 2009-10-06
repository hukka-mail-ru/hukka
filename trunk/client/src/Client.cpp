/*====================================================================================================
//    ___ _ _            _                          
//   / __\ (_) ___ _ __ | |_        ___ _ __  _ __  
//  / /  | | |/ _ \ '_ \| __|      / __| '_ \| '_ \ 
// / /___| | |  __/ | | | |_   _  | (__| |_) | |_) |
// \____/|_|_|\___|_| |_|\__| (_)  \___| .__/| .__/ 
//                                     |_|   |_|   
====================================================================================================*/

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
#define LOGIN_STATUS                    1

/*====================================================================================================
  __  ___  ____     __  ___   __ 
 / _)(  _)(_  _)   / _)(  ,) / _)
( (/\ ) _)  )(    ( (_  )  \( (_ 
 \__/(___) (__)    \__)(_)\_)\__)
====================================================================================================*/
char getCRC(const QByteArray& data)
{
        char crc = 0;
        for(int i = 0; i < data.size(); i++) {
                crc ^= data[i];
        }

        return crc;
}

   
/*==================================================================================================== 
  __  __  _  _  ___  ____  ___  _  _  __  ____  __  ___  
 / _)/  \( \( )/ __)(_  _)(  ,)( )( )/ _)(_  _)/  \(  ,) 
( (_( () ))  ( \__ \  )(   )  \ )()(( (_   )( ( () ))  \ 
 \__)\__/(_)\_)(___/ (__) (_)\_)\__/ \__) (__) \__/(_)\_)
====================================================================================================*/    
Client::Client(): mStatus(CLI_OFFLINE) 
{
//        int metaType=qRegisterMetaType<QAbstractSocket::SocketError>("QAbstractSocket::SocketError"); 

        QObject::connect(&mSocket, SIGNAL(connected()), this, SLOT(onConnected()));
        QObject::connect(&mSocket, SIGNAL(error(QAbstractSocket::SocketError)), this, SLOT(onError()));
        //connect(service,SIGNAL(readyRead()),SLOT(parse()));
}
/*==================================================================================================== 
  __  __  _  _  _  _  ___   __  ____    ____  __     _  _  __   ___  ____ 
 / _)/  \( \( )( \( )(  _) / _)(_  _)  (_  _)/  \   ( )( )/  \ / __)(_  _)
( (_( () ))  (  )  (  ) _)( (_   )(      )( ( () )   )__(( () )\__ \  )(  
 \__)\__/(_)\_)(_)\_)(___) \__) (__)    (__) \__/   (_)(_)\__/ (___/ (__) 
====================================================================================================*/    
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

/*==================================================================================================== 
 ___  __  ___   __  __  _  _  _  _  ___   __  ____    ___  ___   __  __  __    _  _  __   ___  ____ 
(   \(  )/ __) / _)/  \( \( )( \( )(  _) / _)(_  _)  (  _)(  ,) /  \(  \/  )  ( )( )/  \ / __)(_  _)
 ) ) ))( \__ \( (_( () ))  (  )  (  ) _)( (_   )(     ) _) )  \( () ))    (    )__(( () )\__ \  )(  
(___/(__)(___/ \__)\__/(_)\_)(_)\_)(___) \__) (__)   (_)  (_)\_)\__/(_/\/\_)  (_)(_)\__/ (___/ (__) 
====================================================================================================*/  
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

/*==================================================================================================== 
 __    __    __  __  _  _ 
(  )  /  \  / _)(  )( \( )
 )(__( () )( (/\ )(  )  ( 
(____)\__/  \__/(__)(_)\_)
====================================================================================================*/
void Client::login(const QString& username, const QString& passwd) 
{ 
try
{
        if(mSocket.state() != QAbstractSocket::ConnectedState) {
                THROW_EXCEPTION("Connection has been lost");   
        }

        // send LOGIN command
        QByteArray data = (username + QChar(0) + passwd).toAscii();
        sendCmd(SRV, CMD_LOGIN, data);
        
        // get server reply
        ErrorMessage message = getReply(SRV, LOGIN_STATUS);

        switch(message->error) {
                case NOERR:          break;
                case ERRUSERONLINE:  qDebug() << "User"<< username << "is already online"; return; break;
                case ERRBADLOGIN:    THROW_EXCEPTION(LOGIN_ERROR_HEAD + "Incorrect user name: " + username); break;
                case ERRBADPASS:     THROW_EXCEPTION(LOGIN_ERROR_HEAD + "Incorrect password for user: " + username); break;
                default:             THROW_EXCEPTION(LOGIN_ERROR_HEAD + "Internal server error " + (int)message->header.cmd); break;
        } 
} catch (Exception& e) {
        qDebug() << "Can't login to server" << mSocket.peerName() << ". " << e.what();
        throw e;
}
        	
}


/*==================================================================================================== 
 ___  ___  _  _  ___      __  __  __  ___  
/ __)(  _)( \( )(   \    / _)(  \/  )(   \ 
\__ \ ) _) )  (  ) ) )  ( (_  )    (  ) ) )
(___/(___)(_)\_)(___/    \__)(_/\/\_)(___/ 
====================================================================================================*/
// Arrange a packet and write it to Socket
void Client::sendCmd(char service, char command, const QByteArray& data)
{
        char crc = 0;

        MessageHeader header;
        header.sign    = PROTOCOL_SIGNATURE;
        header.version = PROTOCOL_VERSION;
        header.address = service;
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
/*
for(int i=0; i<message.size(); i++) {
	char c = message[i];
	qDebug() << (int)c;
}
*/
        qDebug() << "Sending command with id" << (int)command;  
        qint64 bytes = mSocket.write(message);
        if(bytes == -1) {
                THROW_EXCEPTION("Can't send command  to server " + mSocket.peerName() + ". Command ID: " + (int)command);   
        }

        Q_ASSERT(bytes == message.size());

	qDebug() << "OK" << bytes << "bytes sent";   
}


/*==================================================================================================== 
  __  ___  ____    ___   ___  ___  __    _  _ 
 / _)(  _)(_  _)  (  ,) (  _)(  ,\(  )  ( \/ )
( (/\ ) _)  )(     )  \  ) _) ) _/ )(__  \  / 
 \__/(___) (__)   (_)\_)(___)(_)  (____)(__/  
====================================================================================================*/
ErrorMessage Client::getReply(char service, char reply)
{
        if(!mSocket.waitForReadyRead(WAIT_RESPONSE_TIMEOUT*1000)) {
                THROW_EXCEPTION("Server does't respond.");   
        }

	QByteArray buf = mSocket.readAll();
	qDebug() << "Server replied"<< buf.size() << "bytes";   

        ErrorMessage* message = (ErrorMessage*)buf.data();

	if(message->header.sign != PROTOCOL_SIGNATURE) {
                THROW_EXCEPTION("Server uses wrong protocol ");   
        }

        QByteArray infPart((char*)&message->header.version, message->header.size - sizeof(message->crc));
        if(message->crc != getCRC(infPart)) {
                THROW_EXCEPTION("Server response has bad CRC"); 
        }

	if(message->header.version != PROTOCOL_VERSION) {
                THROW_EXCEPTION("Server uses wrong protocol version: " + (int)message->header.version); 
        }

	if(message->header.address != service) {
                THROW_EXCEPTION("Server uses wrong service: " + message->header.address); 
        }

	if(message->header.cmd != reply) {
                THROW_EXCEPTION("Server returns wrong reply: " + message->header.cmd + " (expected " + LOGIN_STATUS + ")"); 
        }

        return message;
}


/*==================================================================================================== 
 ___  ____  __  ____  _  _  ___ 
/ __)(_  _)(  )(_  _)( )( )/ __)
\__ \  )(  /__\  )(   )()( \__ \
(___/ (__)(_)(_)(__)  \__/ (___/
====================================================================================================*/
ClientStatus Client::status() 
{ 
        if(mSocket.state() == QAbstractSocket::ConnectedState)
                return CLI_ONLINE;

        return CLI_OFFLINE; 
}

/*==================================================================================================== 
  __  _  _     __  __  _  _  _  _  ___   __  ____  ___  ___  
 /  \( \( )   / _)/  \( \( )( \( )(  _) / _)(_  _)(  _)(   \ 
( () ))  (   ( (_( () ))  (  )  (  ) _)( (_   )(   ) _) ) ) )
 \__/(_)\_)   \__)\__/(_)\_)(_)\_)(___) \__) (__) (___)(___/ 
====================================================================================================*/
void Client::onConnected()
{
        qDebug() << "SIGNAL connected";
}

/*==================================================================================================== 
  __  _  _    ___  ___   ___   __  ___  
 /  \( \( )  (  _)(  ,) (  ,) /  \(  ,) 
( () ))  (    ) _) )  \  )  \( () ))  \ 
 \__/(_)\_)  (___)(_)\_)(_)\_)\__/(_)\_)
====================================================================================================*/
void Client::onError()
{
        qDebug() << "SIGNAL error";
}







