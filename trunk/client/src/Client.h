#ifndef CLIENT_H_
#define CLIENT_H_

#include <QObject>
#include <QNetworkProxy>
#include <QTcpSocket>


struct MessageHeader
{
        char            sign;
        quint32         size;
        char            version;
        quint32         address;
        char            cmd;
};

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
        LOG_USER_ONLINE = 3,
        LOG_ERROR = 0
};

class Client: public QObject
{
Q_OBJECT
public:
        Client();

        bool connectToHost(const QNetworkProxy& proxy, const QString& hostName, quint16 port);
        bool disconnectFromHost();
        ClientStatus status();

        LogStatus login(const QString& username, const QString& passwd);
        LogStatus logout(const QString& username);


private:
        enum Command
        {
                CMD_LOGIN,
                CMD_MY_DISCONNECT
        };

        enum Response
        {
                RSP_LOGIN_STATUS = 1,
                RSP_CLOSE_CONNECTION = 2
        };

        bool sendCmd(Command command, const QByteArray& data);        

        QTcpSocket mSocket;
        ClientStatus mStatus;

private slots:
        
        void onConnected();
        void onError();         


};


#endif /*CLIENT_H_*/
