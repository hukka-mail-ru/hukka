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
#include <tablemanager/tbmdefs.h>

using namespace std;

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
Client::Client(): mStatus(CLI_DISCONNECTED) 
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
                THROW_EXCEPTION("Connection failed! Can't connect to server: " + hostName + ".");
        }

        mStatus = CLI_CONNECTED;
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
                qDebug() << "Client has already disconnected.";   
                return;
        }

        mSocket.disconnectFromHost();

        qDebug() << "waiting... ";
        if(mSocket.state() != QAbstractSocket::UnconnectedState &&
          !mSocket.waitForDisconnected(WAIT_CONNECT_TIMEOUT*1000)) {  
                THROW_EXCEPTION("Can't disconnect from server: " + mSocket.peerName() + ".");              
        }

        mStatus = CLI_DISCONNECTED;
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
	try {
		// send command
		QByteArray data = (username + QChar(0) + passwd).toAscii();
		sendCmd(SRV, CMD_LOGIN, data);
		
		// get server reply
		QByteArray message = getReply(SRV, LOGIN_STATUS);
                Q_ASSERT(message.size() == sizeof(char));

		switch(message[0]) {
		        case NOERR:          break;
		        case ERRUSERONLINE:  THROW_EXCEPTION("User '" + username + "' is already online."); break;
		        case ERRBADLOGIN:    THROW_EXCEPTION("Incorrect user name: '" + username + "'."); break;
		        case ERRBADPASS:     THROW_EXCEPTION("Incorrect password for user: '" + username + "'."); break;
		        default:             THROW_EXCEPTION("Internal server error " + QString::number(message[0]) + "."); break;
		}                 
                mStatus = CLI_AUTHORIZED;
	} 
        catch (Exception& e) {
                e.add("Can't login to server " + mSocket.peerName() + ". ");
		qDebug() << e.what();
		throw e;
	}        	
}

/*==================================================================================================== 
 ___   ___   __  __  ___  ____  ___  ___     _  _  ___  ___  ___  
(  ,) (  _) / _)(  )/ __)(_  _)(  _)(  ,)   ( )( )/ __)(  _)(  ,) 
 )  \  ) _)( (/\ )( \__ \  )(   ) _) )  \    )()( \__ \ ) _) )  \ 
(_)\_)(___) \__/(__)(___/ (__) (___)(_)\_)   \__/ (___/(___)(_)\_)
====================================================================================================*/
void Client::registerUser(const QString& username, const QString& passwd) 
{ 
	try {
		// send command
		QByteArray data = (username + QChar(0) + passwd).toAscii();
		sendCmd(REG, CMD_REG, data);
		
		// get server reply
		QByteArray message = getReply(REG, REG_STATUS);
                Q_ASSERT(message.size() == sizeof(char));

		switch(message[0]) {
		        case NOERR:          break;
		        case ERRBADLOGIN:    THROW_EXCEPTION("Incorrect user name: '" + username + "'."); break;
		        case ERRBADPASS:     THROW_EXCEPTION("Incorrect password for user: '" + username + "'."); break;
		        case ERRLOGINEXIST:  THROW_EXCEPTION("User: '" + username + "' already exists."); break;
		        default:             THROW_EXCEPTION("Internal server error " + QString::number(message[0]) + "."); break;
		} 
	} catch (Exception& e) {
                e.add("Can't register user on server " + mSocket.peerName() + ". ");
		qDebug() << e.what();
		throw e;
	}        	
}


/*==================================================================================================== 
  __  ___   ___   __  ____  ___     __   __   __  __  ___    ____  __   ___  __    ___ 
 / _)(  ,) (  _) (  )(_  _)(  _)   / _) (  ) (  \/  )(  _)  (_  _)(  ) (  ,)(  )  (  _)
( (_  )  \  ) _) /__\  )(   ) _)  ( (/\ /__\  )    (  ) _)    )(  /__\  ) ,\ )(__  ) _)
 \__)(_)\_)(___)(_)(_)(__) (___)   \__/(_)(_)(_/\/\_)(___)   (__)(_)(_)(___/(____)(___)
====================================================================================================*/
quint32 Client::createGameTable(quint32 logicID, quint32 timeToStep, quint32 timeToGame, 
                                quint32 minRating, quint32 maxRating)
{
        quint32 tableId = 0;
	try {
                if(mStatus != CLI_AUTHORIZED) {
                        THROW_EXCEPTION("Client is not authorized");                 
                }

		// send command
                quint32 params[] = { logicID, 
                             PARAMETER_ID_TIME2STEP, timeToStep, 
                             PARAMETER_ID_TIME2GAME, timeToGame,
                             PARAMETER_ID_MINRATING, minRating, 
                             PARAMETER_ID_MAXRATING, maxRating };

		QByteArray data = QByteArray((char*)&params, sizeof(params));
		sendCmd(TBM, CMD_CREATE, data);

                // get server reply
                struct Reply {
                        quint32         tableID;
	                char            isValid;
                };
		
		QByteArray message = getReply(TBM, ANS_CREATE);
                Reply* reply = (Reply*)message.data();

		switch(reply->isValid) {
		        case ST_VALID:       break;
		        case ST_NOTVALID:    THROW_EXCEPTION("Invalid parameter(s)"); break; // TODO What parameter is wrong (exactly)?
		        default:             THROW_EXCEPTION("Internal server error" + QString::number(reply->isValid)); break;
		} 
                tableId = reply->tableID;
	} 
        catch (Exception& e) {
                e.add("Can't create Game Table on server: " + mSocket.peerName() + ". ");
		qDebug() << e.what();
		throw e;
	}        

        return tableId;
}

/*==================================================================================================== 
  __  ___  ____    __  __  _  _     __   __   __  __  ___    ____  __   ___  __    ___ 
 / _)(  _)(_  _)  (  \/  )( \/ )   / _) (  ) (  \/  )(  _)  (_  _)(  ) (  ,)(  )  (  _)
( (/\ ) _)  )(     )    (  \  /   ( (/\ /__\  )    (  ) _)    )(  /__\  ) ,\ )(__  ) _)
 \__/(___) (__)   (_/\/\_)(__/     \__/(_)(_)(_/\/\_)(___)   (__)(_)(_)(___/(____)(___)
====================================================================================================*/
quint32 Client::getMyGameTable(quint32 logicID)
{
        quint32 tableId = 0;
	try {
                if(mStatus != CLI_AUTHORIZED) {
                        THROW_EXCEPTION("Client is not authorized");                 
                }

		QByteArray data = QByteArray((char*)&logicID, sizeof(logicID));
		sendCmd(TBM, CMD_GETMYTBL, data);

                // get server reply
                struct Reply {
                        quint32         tableID;
	                char            isValid;
                };

                QByteArray message = getReply(TBM, ANS_MYTBL);
                Reply* reply = (Reply*)message.data();

		switch(reply->isValid) {
		        case ST_VALID:       tableId = reply->tableID;  break;
		        case ST_NOTVALID:    tableId = 0;  break;
		        default:             THROW_EXCEPTION("Internal server error" + QString::number(reply->isValid)); break;
		} 
	} 
        catch (Exception& e) {
                e.add("Can't get my Game Table on server: " + mSocket.peerName() + ". ");
		qDebug() << e.what();
		throw e;
	}        

        return tableId;
}

/*==================================================================================================== 
 ___  ___  __    ___  ____  ___     __   __   __  __  ___    ____  __   ___  __    ___ 
(   \(  _)(  )  (  _)(_  _)(  _)   / _) (  ) (  \/  )(  _)  (_  _)(  ) (  ,)(  )  (  _)
 ) ) )) _) )(__  ) _)  )(   ) _)  ( (/\ /__\  )    (  ) _)    )(  /__\  ) ,\ )(__  ) _)
(___/(___)(____)(___) (__) (___)   \__/(_)(_)(_/\/\_)(___)   (__)(_)(_)(___/(____)(___)
====================================================================================================*/
void Client::deleteGameTable(quint32 logicID, quint32 tableID)
{
	try {
                if(mStatus != CLI_AUTHORIZED) {
                        THROW_EXCEPTION("Client is not authorized");                 
                }

                quint32 params[] = { logicID, tableID };
		QByteArray data = QByteArray((char*)&params, sizeof(params));
		sendCmd(TBM, CMD_DELETE, data);

                // get server reply
                struct Reply {
                        quint32         logicID;
                        quint32         tableID;
	                char            isValid;
                };

                QByteArray message = getReply(TBM, ANS_DELETE);
                Reply* reply = (Reply*)message.data();

		switch(reply->isValid) {
		        case ST_VALID:       break;
		        case ST_NOTVALID:    THROW_EXCEPTION("Invalid parameter. Attempt to delete wrong Game Table?");  break;
		        default:             THROW_EXCEPTION("Internal server error" + QString::number(reply->isValid)); break;
		} 
	} 
        catch (Exception& e) {
                e.add("Can't delete Game Table on server: " + mSocket.peerName() + ". ");
		qDebug() << e.what();
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
	if(mSocket.state() != QAbstractSocket::ConnectedState) {
	        THROW_EXCEPTION("Connection has been lost.");   
	}

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

        qDebug() << "Sending command with id" << (int)command;  
        qint64 bytes = mSocket.write(message);
        if(bytes == -1) {
                THROW_EXCEPTION("Can't send command with ID: " + QString::number(command) + ".");   
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
QByteArray Client::getReply(quint32 service, char reply)
{
	if(mSocket.state() != QAbstractSocket::ConnectedState) {
	        THROW_EXCEPTION("Connection has been lost.");   
	}

        if(!mSocket.waitForReadyRead(WAIT_RESPONSE_TIMEOUT*1000)) {
                THROW_EXCEPTION("Server does't respond.");   
        }

	QByteArray buf = mSocket.readAll();
        MessageHeader* header = (MessageHeader*)buf.data();

QString str = "SERVER REPLY: ";
for(int i=0; i<buf.size(); i++)
        str += QString::number((int)buf[i]) + " ";
qDebug() << str;

	if(header->sign != PROTOCOL_SIGNATURE) {
                THROW_EXCEPTION("Server uses wrong protocol ");   
        }

        QByteArray infPart((char*)&header->version, header->size - CRC_SIZE);
        if(buf[buf.size() - CRC_SIZE] != getCRC(infPart)) {
                THROW_EXCEPTION("Server response has bad CRC"); 
        }

	if(header->version != PROTOCOL_VERSION) {
                THROW_EXCEPTION("Server uses wrong protocol version: " + QString::number(header->version)); 
        }

	if(header->address != service) {
                THROW_EXCEPTION("Server uses wrong service: " + header->address); 
        }

	if(header->cmd != reply) {
                THROW_EXCEPTION("Server returns wrong reply: " + QString::number(header->cmd)); 
        }

        char* dataOffset = buf.data() + sizeof(MessageHeader);
        unsigned dataSize = header->size - sizeof(header->version) - sizeof(header->address) - sizeof(header->cmd) - CRC_SIZE;

        return QByteArray(dataOffset, dataSize);
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
        THROW_EXCEPTION("Socket error");
}







