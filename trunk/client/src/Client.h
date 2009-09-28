#ifndef CLIENT_H_
#define CLIENT_H_

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

class Client
{
public:
        Client(): mStatus(CLI_OFFLINE) {}

        ClientStatus connect(const QNetworkProxy& proxy, const QString& address, quint16 port);
        ClientStatus disconnect();
        ClientStatus status() { return mStatus; }

        LogStatus login(const QString& username, const QString& passwd);
        LogStatus logout(const QString& username);
 

private:

        QTcpSocket mSocket;
        ClientStatus mStatus;

        // getCRC()
        // send()
        // receive()


};


#endif /*CLIENT_H_*/
