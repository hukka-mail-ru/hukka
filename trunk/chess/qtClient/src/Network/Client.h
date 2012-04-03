#ifndef CLIENT_H_
#define CLIENT_H_


#include <QObject>
#include <QtNetwork/QNetworkProxy>
#include <QtNetwork/QTcpSocket>
#include <QThread>
#include <QList>
#include <QReadWriteLock>
#include <QLinkedList>
#include <deferror.h>
#include <Global.h>
#include "Pixmaps.h"


enum GameStatus
{
    GAM_STOPPED,
    GAM_OPPONENT_JOINED,
    GAM_STARTED
};



#pragma pack(1)


struct Param
{
        quint32         id;
        quint32         value;
        quint32         compareOperator;
        quint32         logicalOperator;
};




/*====================================================================================================
  __  __    __  ___  _  _  ____
 / _)(  )  (  )(  _)( \( )(_  _)
( (_  )(__  )(  ) _) )  (   )(
 \__)(____)(__)(___)(_)\_) (__)
====================================================================================================*/
class Client: public QObject
{
Q_OBJECT
friend class TestClient;
friend class TestClientChess;

        Client(const QString& name = "");

public:
        static Client* instance()
        {
            static Client* client = new Client();
            return client;
        }

        void connectToHost(const QNetworkProxy& proxy, const QString& hostName, quint16 port);
        void disconnectFromHost();

        // communication with SRV Server
        void login(const QString& username, const QString& passwd);
        void registerUser(const QString& username, const QString& passwd);

        // communication with Table Manager
        void createGameTable     (LOGICID logicID, const QList<Param>& params);
        void deleteGameTable     (LOGICID logicID, TABLEID tableID);

        void getMyGameTable      (LOGICID logicID);
        void getRandomGameTable  (LOGICID logicID, const QList<Param>& params);
        void getGameTableParams  (LOGICID logicID, TABLEID tableID);
    //  void setGameTableParam   (LOGICID logicID, TABLEID tableID, const Param& param); // not implemented on server
    //  void getGameTableParams  (LOGICID logicID, TABLEID tableID, const QList<PARAMID>& ids);
    //  void setGameTableParams  (LOGICID logicID, TABLEID tableID, const QList<Param>& params);
        void findGameTables      (LOGICID logicID, quint32 maxCount, const QList<Param>& params);
        void replenishBalance    (const QString& pin);

        // communication with Chess Server
        // Master
        void approveGame         (TABLEID tableID);
        void rejectGame          (TABLEID tableID);
        // Slave
        void joinGame            (TABLEID tableID);
        // Both
        void getPosition         (TABLEID tableID);
        void move                (TABLEID tableID, const Move& move, piece_type promotion = Empty);
        void surrender           (TABLEID tableID);
      //  void getTime             (TABLEID tableID);
        void timeout             (TABLEID tableID);
        void getMyRating         ();
     //   void getMyBalance        ();
        void getOpponent         (TABLEID tableID);
        void getLastGameResult   ();
        void deleteLastGameResult();

        // draw
        void offerDraw           (TABLEID tableID);
        void replyDraw           (TABLEID tableID, bool agree);

        // communication with Chat Server
        void joinChat           (LOGICID logicID, TABLEID tableID);
        void leaveChat          (LOGICID logicID);
        void sendChatMessage    (LOGICID logicID, TABLEID tableID, const QString& message);
   //     void deleteChatHistory  (LOGICID logicID, TABLEID tableID);


        GameStatus   getGameStatus()   { return mGameStatus; }
        void         setGameStatus(GameStatus status)   { mGameStatus = status; }
        QString      username()        { return mUsername; }

signals:

        void connectedToHost();
        void disconnectedFromHost();
        void authorized();
        void registered();
        void joined(TABLEID id);

        void opponentJoined(const Player& opponent);
        void gotOpponent(const Player& opponent);

        void gameStarted();
        void gameRejected();
        void gameOver(int status, int rating);
        void gameTableCreated(TABLEID id);
        void gameTableDeleted();

        void drawOffered();
        void drawRejected(const QString& message);

        void gotMyGameTable(TABLEID id, bool isOwner);
        void gotGameTables(const QList<GameTable>& tables);
        void gotPosition(const Position& position);
        void gotGameTableParams(const GameTable& table);

        void gotMoveTime(unsigned);
        void gotGameTime(unsigned);

        void gotMyRating(unsigned rating, unsigned balance);
     //   void gotMyBalance(unsigned);
        void gotLastGameResult(unsigned);

        void balanceReplenished(unsigned);

        void invalidMove();

        void chatMessage(const QString& message);
        void chatUserOnline(const QString& userName);
        void chatUserJoined(const QString& userName);
        void chatUserLeft(const QString& userName);

        void error(const QString& what);
        void notAuthorized(const QString& what);

private:

        struct MessageHeader
        {
                char            sign;
                qint32          size;
                char            version;
                quint32         service;
                char            cmd;
        };

        struct Message
        {
            char cmd;
            quint32 service;
            QByteArray buffer;
        };

        void sendCmd(char service, char command, const QByteArray& data);

        void processMessageSRV (const MessageHeader& header, const QByteArray& buffer);
        void processMessageCHS (const MessageHeader& header, const QByteArray& buffer);
        void processMessageTBM (const MessageHeader& header, const QByteArray& buffer);
        void processMessageREG (const MessageHeader& header, const QByteArray& buffer);
        void processMessageCHAT(const MessageHeader& header, const QByteArray& buffer);

        QString mName;
        QString mUsername;

        QTcpSocket mSocket;

        GameStatus   mGameStatus;

        bool mClientAuthorized;

        QTimer* mTimer;

private slots:

        void onConnected();
        void onDisconnected();
        void onError();
        void onReadyRead();
        void onTimeout();
};



#endif /*CLIENT_H_*/
