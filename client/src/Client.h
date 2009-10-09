#ifndef CLIENT_H_
#define CLIENT_H_

#include <QObject>
#include <QNetworkProxy>
#include <QTcpSocket>
#include <deferror.h>

#define WAIT_CONNECT_TIMEOUT 3 // seconds
#define WAIT_RESPONSE_TIMEOUT 3 // seconds

#define PROTOCOL_SIGNATURE		'Z'
#define PROTOCOL_VERSION                2

#define CRC_SIZE                        sizeof(char)

// services (see wsUsers table)  
#define	SRV		1
#define	REG		2
#define TBM		4

// server commands
#define CMD_LOGIN                       1
#define CMD_REG				1

// server replies
#define LOGIN_STATUS                    1
#define REG_STATUS 			1

// game logic ids (see tbLogicList table)
#define LOGIC_ID_GAMMON 		1
#define LOGIC_ID_CHESS 			2

// game table parameter ids (see tbParameterList table)
#define PARAMETER_ID_PASSWD           2
#define PARAMETER_ID_TIME2STEP        3
#define PARAMETER_ID_TIME2GAME        4
#define PARAMETER_ID_MINRATING        5
#define PARAMETER_ID_MAXRATING        6

// game table parameter constrains (see tbParamList table)
#define PARAMETER_MAX_TIME2STEP       420
#define PARAMETER_MAX_TIME2GAME       7200

enum ClientStatus
{
        CLI_CONNECTED,   
        CLI_DISCONNECTED,
        CLI_AUTHORIZED 
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
        quint32 createGameTable(quint32 logicID, 
                                quint32 timeToStep = PARAMETER_MAX_TIME2STEP, 
                                quint32 timeToGame = PARAMETER_MAX_TIME2GAME, 
                                quint32 minRating = 0, 
                                quint32 maxRating = 0);

        quint32 getMyGameTable(quint32 logicID);
        void deleteGameTable(quint32 logicID, quint32 tableID);

        ClientStatus status() { return mStatus; }

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
