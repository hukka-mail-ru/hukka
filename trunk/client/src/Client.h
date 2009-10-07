#ifndef CLIENT_H_
#define CLIENT_H_

#include <QObject>
#include <QNetworkProxy>
#include <QTcpSocket>
#include <deferror.h>
#include <defserver.h>

#define LOGIC_ID_GAMMON 		1
#define LOGIC_ID_CHESS 			2

enum ClientStatus
{
        CLI_ONLINE,
        CLI_OFFLINE,
};

char getCRC(const QByteArray& data);


#pragma pack(1)
struct MessageHeader
{
	char            sign;
	qint32	        size;
	char            version;
	quint32         address;
	char            cmd;
};

/*====================================================================================================
    ___ _ _            _   
   / __\ (_) ___ _ __ | |_ 
  / /  | | |/ _ \ '_ \| __|
 / /___| | |  __/ | | | |_ 
 \____/|_|_|\___|_| |_|\__|
====================================================================================================*/
class Client: public QObject
{
Q_OBJECT
friend class TestClient;

public:
        Client();

        void connectToHost(const QNetworkProxy& proxy, const QString& hostName, quint16 port);
        void disconnectFromHost();

        void login(const QString& username, const QString& passwd);
        void registerUser(const QString& username, const QString& passwd);

        // returns table id
        quint32 createGameTable(quint32 logicID, quint32 timeToStep = INT_MAX, quint32 timeToGame = INT_MAX, 
                                quint32 minRating = 0, quint32 maxRating = INT_MAX);

        ClientStatus status();

private:

        void sendCmd(char service, char command, const QByteArray& data);   

        // returns data of message (all except MessageHeader and CRC)
	QByteArray getReply(quint32 service, char reply);

        QTcpSocket mSocket;
        ClientStatus mStatus;

private slots:
        
        void onConnected();
        void onError();         


};


#endif /*CLIENT_H_*/
