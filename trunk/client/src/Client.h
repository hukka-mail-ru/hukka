#ifndef CLIENT_H_
#define CLIENT_H_

#include <QObject>
#include <QNetworkProxy>
#include <QTcpSocket>
#include <deferror.h>
#include <defserver.h>

enum ClientStatus
{
        CLI_ONLINE,
        CLI_OFFLINE,
};

char getCRC(const QByteArray& data);

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

        ClientStatus status();

private:
	#pragma pack(1)
	struct MessageHeader
	{
		char            sign;
		qint32	        size;
		char            version;
		quint32         address;
		char            cmd;
	};
        struct ErrorMessage
        {
                char            error;
                char            crc;
        };

        void sendCmd(char command, const QByteArray& data);   

        QTcpSocket mSocket;
        ClientStatus mStatus;

private slots:
        
        void onConnected();
        void onError();         


};


#endif /*CLIENT_H_*/
