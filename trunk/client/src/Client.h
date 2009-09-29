#ifndef CLIENT_H_
#define CLIENT_H_

#include <QObject>
#include <QNetworkProxy>
#include <QTcpSocket>


enum ClientStatus
{
        CLI_ONLINE,
        CLI_OFFLINE,
};

enum LogStatus
{
        LOG_OK = 0,
        LOG_WRONG_USER = 1,
        LOG_WRONG_PASSWD = 2,
        LOG_USER_ONLINE = 3
};

class Client: public QObject
{
Q_OBJECT
public:
        Client();

        ClientStatus connectToHost(const QNetworkProxy& proxy, const QString& hostName, quint16 port);
        ClientStatus disconnectFromHost();
        ClientStatus status();

        LogStatus login(const QString& username, const QString& passwd);
        LogStatus logout(const QString& username);


private:

        QTcpSocket mSocket;
        ClientStatus mStatus;

        // getCRC()
        // send()
        // receive()

private slots:
        
        void onConnected();
        void onError();         

};


#endif /*CLIENT_H_*/
