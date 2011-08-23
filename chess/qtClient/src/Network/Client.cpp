/*====================================================================================================
//    ___ _ _        _
//   / __\ (_) ___ _ __ | |_        ___ _ __  _ __
//  / /  | | |/ _ \ '_ \| __|      / __| '_ \| '_ \
// / /___| | |  __/ | | | |_   _  | (__| |_) | |_) |
// \____/|_|_|\___|_| |_|\__| (_)  \___| .__/| .__/
//                                     |_|   |_|
====================================================================================================*/

#include "Client.h"
#include "Exception.h"
#include <assert.h>
#include <QtNetwork/QNetworkProxyQuery>
#include <QtEndian>
#include <QTime>
#include <QTimer>
#include <tablemanager/tbmdefs.h>
#include <header/defservice.h>
#include <chatserver/chatdefs.h>


#define QT_TRACEOUT ; // qDebug() << "     TRACE " << mName <<  "::" << __FUNCTION__ ;



/*====================================================================================================
  __  __  _  _  ___  ____  ___  _  _  __  ____  __  ___
 / _)/  \( \( )/ __)(_  _)(  ,)( )( )/ _)(_  _)/  \(  ,)
( (_( () ))  ( \__ \  )(   )  \ )()(( (_   )( ( () ))  \
 \__)\__/(_)\_)(___/ (__) (_)\_)\__/ \__) (__) \__/(_)\_)
====================================================================================================*/
Client::Client(const QString& name):
    mName(name),
    mGameStatus(GAM_STOPPED),
    mClientAuthorized(false),
    mTimer(NULL)
{
    QT_TRACEOUT;
//    int metaType=qRegisterMetaType<QAbstractSocket::SocketError>("QAbstractSocket::SocketError");

    QObject::connect(&mSocket, SIGNAL(connected()), this, SLOT(onConnected()));
    QObject::connect(&mSocket, SIGNAL(disconnected()), this, SLOT(onDisconnected()));
    QObject::connect(&mSocket, SIGNAL(readyRead()), this, SLOT(onReadyRead()));
    QObject::connect(&mSocket, SIGNAL(error(QAbstractSocket::SocketError)), this, SLOT(onError()));

}




/*====================================================================================================
  __  __  _  _  _  _  ___   __  ____    ____  __     _  _  __   ___  ____
 / _)/  \( \( )( \( )(  _) / _)(_  _)  (_  _)/  \   ( )( )/  \ / __)(_  _)
( (_( () ))  (  )  (  ) _)( (_   )(      )( ( () )   )__(( () )\__ \  )(
 \__)\__/(_)\_)(_)\_)(___) \__) (__)    (__) \__/   (_)(_)\__/ (___/ (__)
====================================================================================================*/
void Client::connectToHost(const QNetworkProxy& proxy, const QString& hostName, quint16 port)
{
    qDebug() << "Client::connectToHost. mSocket.state() =" << mSocket.state();

    QT_TRACEOUT;
    if(mSocket.state() == QAbstractSocket::ConnectedState)
     //  mSocket.state() == QAbstractSocket::ConnectingState)
    {
        emit connectedToHost();
    }
    else
    {
     //     mSocket.setProxy(proxy);

        mTimer = new QTimer(this);
     //   connect(mTimer, SIGNAL(timeout()), this, SLOT(onTimeout()));
        mTimer->start(WAIT_CONNECT_TIMEOUT * 1000);

        mSocket.connectToHost (hostName, port);
        //qDebug() << "Proxy: " << mSocket.proxy().hostName() << ":" << mSocket.proxy().port() << " type=" << mSocket.proxy().type();
        //qDebug() << "connecting to host: " << hostName << ":" << port;
    }

}


/*====================================================================================================
 ___  __  ___   __  __  _  _  _  _  ___   __  ____    ___  ___   __  __  __    _  _  __   ___  ____
(   \(  )/ __) / _)/  \( \( )( \( )(  _) / _)(_  _)  (  _)(  ,) /  \(  \/  )  ( )( )/  \ / __)(_  _)
 ) ) ))( \__ \( (_( () ))  (  )  (  ) _)( (_   )(     ) _) )  \( () ))    (    )__(( () )\__ \  )(
(___/(__)(___/ \__)\__/(_)\_)(_)\_)(___) \__) (__)   (_)  (_)\_)\__/(_/\/\_)  (_)(_)\__/ (___/ (__)
====================================================================================================*/
void Client::disconnectFromHost()
{
    QT_TRACEOUT;

    if(mSocket.state() != QAbstractSocket::ConnectedState)
    {
//        qDebug() << "emit disconnectedFromHost();";
        emit disconnectedFromHost();
    }

    //qDebug() << "mSocket.disconnectFromHost()";

    ///mSocket.disconnectFromHost(); // <- for some unknown reasons the player stays on server (server then reports "Player is already online")
    mSocket.flush();
    emit disconnectedFromHost();
}

/*====================================================================================================
 __    __    __  __  _  _
(  )  /  \  / _)(  )( \( )
 )(__( () )( (/\ )(  )  (
(____)\__/  \__/(__)(_)\_)
====================================================================================================*/
void Client::login(const QString& username, const QString& passwd)
{
    QT_TRACEOUT;

    try
    {
        // send command
        mUsername = username;
        QByteArray data = (username + QChar(0) + passwd).toAscii();
        sendCmd(SRV, CMD_LOGIN, data);
    }
    catch (Exception& e) {
        e.add(tr("Can't login to server ") + mSocket.peerName() + tr(", user: ") + username + ". ");
        emit error (e.what());
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
    QT_TRACEOUT;

    try {
        // send command
        mUsername = username;
        QByteArray data = (username + QChar(0) + passwd).toAscii();
        sendCmd(REG, CMD_REG, data);

    } catch (Exception& e) {
        e.add(tr("Can't register user on server ") + mSocket.peerName() + tr(". User name: ") + username + ". ");
        emit error (e.what());
    }
}


/*====================================================================================================
  __  ___   ___   __  ____  ___     __   __   __  __  ___    ____  __   ___  __    ___
 / _)(  ,) (  _) (  )(_  _)(  _)   / _) (  ) (  \/  )(  _)  (_  _)(  ) (  ,)(  )  (  _)
( (_  )  \  ) _) /__\  )(   ) _)  ( (/\ /__\  )    (  ) _)    )(  /__\  ) ,\ )(__  ) _)
 \__)(_)\_)(___)(_)(_)(__) (___)   \__/(_)(_)(_/\/\_)(___)   (__)(_)(_)(___/(____)(___)
====================================================================================================*/
void Client::createGameTable(LOGICID logicID, const QList<Param>& params)
{
    QT_TRACEOUT;

    try
    {
        assert(mClientAuthorized);

        // send command
        QByteArray data = Q_BYTE_ARRAY(logicID);
        for(int i=0; i<params.size(); i++)
        {
            data += Q_BYTE_ARRAY(params[i].id);
            data += Q_BYTE_ARRAY(params[i].value);
        }

        sendCmd(TBM, CMD_CREATE, data);
    }
    catch (Exception& e) {
        e.add(tr("Can't create Game Table on server: ") + mSocket.peerName() + ". ");
        emit error (e.what());
    }

}

/*====================================================================================================
  __  ___  ____    __  __  _  _     __   __   __  __  ___    ____  __   ___  __    ___
 / _)(  _)(_  _)  (  \/  )( \/ )   / _) (  ) (  \/  )(  _)  (_  _)(  ) (  ,)(  )  (  _)
( (/\ ) _)  )(     )    (  \  /   ( (/\ /__\  )    (  ) _)    )(  /__\  ) ,\ )(__  ) _)
 \__/(___) (__)   (_/\/\_)(__/     \__/(_)(_)(_/\/\_)(___)   (__)(_)(_)(___/(____)(___)
====================================================================================================*/
void Client::getMyGameTable(LOGICID logicID)
{
    QT_TRACEOUT;

    try
    {
        assert(mClientAuthorized);

        // send command
        QByteArray data = Q_BYTE_ARRAY(logicID);
        sendCmd(TBM, CMD_GETMYTBL, data);
    }
    catch (Exception& e) {
        e.add(tr("Can't get my Game Table on server: ") + mSocket.peerName() + ". ");
        qDebug() << e.what();
        emit error (e.what());
    }
}

/*====================================================================================================
  __  ___  ____    ___    __   _  _  ___    __  __  __     __   __   __  __  ___    ____  __   ___  __    ___
 / _)(  _)(_  _)  (  ,)  (  ) ( \( )(   \  /  \(  \/  )   / _) (  ) (  \/  )(  _)  (_  _)(  ) (  ,)(  )  (  _)
( (/\ ) _)  )(     )  \  /__\  )  (  ) ) )( () ))    (   ( (/\ /__\  )    (  ) _)    )(  /__\  ) ,\ )(__  ) _)
 \__/(___) (__)   (_)\_)(_)(_)(_)\_)(___/  \__/(_/\/\_)   \__/(_)(_)(_/\/\_)(___)   (__)(_)(_)(___/(____)(___)
===================================================================================================*/
void Client::getRandomGameTable (LOGICID logicID, const QList<Param>& params)
{
    QT_TRACEOUT;

    try {
        assert(mClientAuthorized);

        // send command
        QByteArray data = Q_BYTE_ARRAY(logicID);
        for(int i=0; i<params.size(); i++) {
            data += Q_BYTE_ARRAY(params[i]);
        }

        sendCmd(TBM, CMD_RANDOM_OP, data);
    }
    catch (Exception& e) {
        e.add(tr("Can't get random Game Table on server: ") + mSocket.peerName() + ". ");
        emit error (e.what());
    }
}

/*====================================================================================================
  __  ___  ____     __   __   __  __  ___    ____  __   ___  __    ___    ___  __   ___    __   __  __
 / _)(  _)(_  _)   / _) (  ) (  \/  )(  _)  (_  _)(  ) (  ,)(  )  (  _)  (  ,\(  ) (  ,)  (  ) (  \/  )
( (/\ ) _)  )(    ( (/\ /__\  )    (  ) _)    )(  /__\  ) ,\ )(__  ) _)   ) _//__\  )  \  /__\  )    (
 \__/(___) (__)    \__/(_)(_)(_/\/\_)(___)   (__)(_)(_)(___/(____)(___)  (_) (_)(_)(_)\_)(_)(_)(_/\/\_)
===================================================================================================*/
void Client::getGameTableParams(LOGICID logicID, TABLEID tableID)
{

    QT_TRACEOUT;

    try {
        assert(mClientAuthorized);

        quint32 time2game = PARAMETER_ID_GAMETIME;
        quint32 time2step = PARAMETER_ID_MOVETIME;
        quint32 playerID  = PARAMETER_ID_PLAYER_NAME;

        // Note! If you add some parameters here, don't forget to change ANS_GET_PARAM!
        QByteArray data = Q_BYTE_ARRAY(logicID) +
                          Q_BYTE_ARRAY(tableID) +
                          Q_BYTE_ARRAY(time2game) +
                          Q_BYTE_ARRAY(time2step) +
                          Q_BYTE_ARRAY(playerID);

        sendCmd(TBM, CMD_GET_PARAMS, data);

    }
    catch (Exception& e) {
        e.add(tr("Can't get Game Table parameter on server: ") + mSocket.peerName() + ". ");
        emit error (e.what());
    }

}

/*====================================================================================================
 ___  ___  ____     __   __   __  __  ___    ____  __   ___  __    ___    ___  __   ___    __   __  __
/ __)(  _)(_  _)   / _) (  ) (  \/  )(  _)  (_  _)(  ) (  ,)(  )  (  _)  (  ,\(  ) (  ,)  (  ) (  \/  )
\__ \ ) _)  )(    ( (/\ /__\  )    (  ) _)    )(  /__\  ) ,\ )(__  ) _)   ) _//__\  )  \  /__\  )    (
(___/(___) (__)    \__/(_)(_)(_/\/\_)(___)   (__)(_)(_)(___/(____)(___)  (_) (_)(_)(_)\_)(_)(_)(_/\/\_)
===================================================================================================*/
/*
void Client::setGameTableParam (LOGICID logicID, TABLEID tableID, const Param& param)
{
    QT_TRACEOUT;

    try {
        if(mClientStatus != CLI_AUTHORIZED) {
            THROW_EXCEPTION("Client is not authorized");
        }

        // send command
        QByteArray data = Q_BYTE_ARRAY(logicID)  + Q_BYTE_ARRAY(tableID) +
                  Q_BYTE_ARRAY(param.id) + Q_BYTE_ARRAY(param.value);
        sendCmd(TBM, CMD_SET_PARAMS, data);

        // get server reply
        struct Reply {
            TABLEID     tableID;
            char        isValid;
        };

        QByteArray message = getMessage(TBM, ANS_SET_PARAMS);
        Reply* reply = (Reply*)message.data();

        switch(reply->isValid) {
            case ST_VALID:       break;
            case ST_NOTVALID:    THROW_EXCEPTION("Invalid parameter.");  break;
            default:         THROW_EXCEPTION("Internal server error " + QString::number(reply->isValid)); break;
        }
        qDebug() << "OK: setGameTableParam. tableId: " << tableID << " paramID: " << param.id;
    }
    catch (Exception& e) {
        e.add("Can't set Game Table parameter on server: " + mSocket.peerName() + ". ");
        qDebug() << e.what();
        emit error (e.what());
    }
}
*/


/*====================================================================================================
 ___  __  _  _  ___      __   __   __  __  ___    ____  __   ___  __    ___  ___
(  _)(  )( \( )(   \    / _) (  ) (  \/  )(  _)  (_  _)(  ) (  ,)(  )  (  _)/ __)
 ) _) )(  )  (  ) ) )  ( (/\ /__\  )    (  ) _)    )(  /__\  ) ,\ )(__  ) _)\__ \
(_)  (__)(_)\_)(___/    \__/(_)(_)(_/\/\_)(___)   (__)(_)(_)(___/(____)(___)(___/
====================================================================================================*/
void Client::findGameTables(LOGICID logicID, quint32 maxCount, const QList<Param>& params)
{
    //qDebug() << "Client::findGameTables";

    QT_TRACEOUT;
    try {
        assert(mClientAuthorized);

        Q_ASSERT(params.last().logicalOperator == OPERATOR_LAST);

        // send command
        QByteArray data = Q_BYTE_ARRAY(logicID) + Q_BYTE_ARRAY(maxCount);
        for(int i=0; i<params.size(); i++) {
            data += Q_BYTE_ARRAY(params[i]);
        }
        sendCmd(TBM, CMD_FIND, data);

    }
    catch (Exception& e) {
        e.add(tr("Can't find Game Tables on server: ") + mSocket.peerName() + ". ");
        emit error (e.what());
    }

}

/*====================================================================================================
 ___  ___  __    ___  ____  ___     __   __   __  __  ___    ____  __   ___  __    ___
(   \(  _)(  )  (  _)(_  _)(  _)   / _) (  ) (  \/  )(  _)  (_  _)(  ) (  ,)(  )  (  _)
 ) ) )) _) )(__  ) _)  )(   ) _)  ( (/\ /__\  )    (  ) _)    )(  /__\  ) ,\ )(__  ) _)
(___/(___)(____)(___) (__) (___)   \__/(_)(_)(_/\/\_)(___)   (__)(_)(_)(___/(____)(___)
====================================================================================================*/

void Client::deleteGameTable(LOGICID logicID, TABLEID tableID)
{
    QT_TRACEOUT;

    try {
        assert(tableID);
        assert(mClientAuthorized);

        // send command
        QByteArray data = Q_BYTE_ARRAY(logicID) + Q_BYTE_ARRAY(tableID);
        //qDebug() << "Client::deleteGameTable" << endl;
        sendCmd(TBM, CMD_DELETE, data);
    }
    catch (Exception& e) {
        e.add(tr("Can't delete Game Table with ID ") + QString::number(tableID) + tr(" on server: ") + mSocket.peerName() + ". ");
        qDebug() << e.what();
        emit error (e.what());
    }
}


/*
void Client::deleteChatHistory(LOGICID logicID, TABLEID tableID)
{
    QT_TRACEOUT;

    try {
        assert(tableID);
        assert(mClientAuthorized);

        // send command
        QByteArray data = Q_BYTE_ARRAY(logicID) + Q_BYTE_ARRAY(tableID);
        //qDebug() << "Client::deleteGameTable" << endl;
        sendCmd(CHAT, CMD_CHAT_DELETE_HISTORY, data);
    }
    catch (Exception& e) {
        e.add(tr("Can't delete Game Table with ID ") + QString::number(tableID) + tr(" on server: ") + mSocket.peerName() + ". ");
        qDebug() << e.what();
        emit error (e.what());
    }
}
*/
/*====================================================================================================
   __  __  __  _  _     __   __   __  __  ___    ____  __   ___  __    ___
  (  )/  \(  )( \( )   / _) (  ) (  \/  )(  _)  (_  _)(  ) (  ,)(  )  (  _)
 __)(( () ))(  )  (   ( (/\ /__\  )    (  ) _)    )(  /__\  ) ,\ )(__  ) _)
(___/ \__/(__)(_)\_)   \__/(_)(_)(_/\/\_)(___)   (__)(_)(_)(___/(____)(___)
====================================================================================================*/
void Client::joinGame (TABLEID tableID)
{
    QT_TRACEOUT;

    try {
        assert(tableID);
        assert(mClientAuthorized);

        // send command
        QByteArray data = Q_BYTE_ARRAY(tableID);
        sendCmd(CHS, CMD_JOIN, data);
    }
    catch (Exception& e) {
        e.add(tr("Can't join Game Table with ID ") + QString::number(tableID) + tr(" on server: ") + mSocket.peerName() + ". ");
        emit error (e.what());
    }

}

void Client::getOpponent (TABLEID tableID)
{
    QT_TRACEOUT;

    try {
        assert(tableID);
        assert(mClientAuthorized);

        // send command
        QByteArray data = Q_BYTE_ARRAY(tableID);
        sendCmd(CHS, CMD_GET_OPPONENT, data);
    }
    catch (Exception& e) {
        e.add(tr("Can't get opponent. Table with ID ") + QString::number(tableID) + tr(" on server: ") + mSocket.peerName() + ". ");
        emit error (e.what());
    }

}


/*====================================================================================================
  ___  ____  __   ___  ____     __   __   __  __  ___
 / __)(_  _)(  ) (  ,)(_  _)   / _) (  ) (  \/  )(  _)
 \__ \  )(  /__\  )  \  )(    ( (/\ /__\  )    (  ) _)
 (___/ (__)(_)(_)(_)\_)(__)    \__/(_)(_)(_/\/\_)(___)
====================================================================================================*/

void Client::approveGame (TABLEID tableID)
{
    QT_TRACEOUT;

    try {
        assert(tableID);
        assert(mClientAuthorized);

        if(mGameStatus != GAM_OPPONENT_JOINED) {
            THROW_EXCEPTION("No opponent joined");
        }

        // send command
        QByteArray data = Q_BYTE_ARRAY(tableID);
        sendCmd(CHS, CMD_OPAGREE, data);
    }
    catch (Exception& e) {
        e.add(tr("Can't agree to start game. Table ID ") + QString::number(tableID) + tr(" on server: ") + mSocket.peerName() + ". ");
        qDebug() << e.what();
        emit error (e.what());
    }
}

void Client::rejectGame (TABLEID tableID)
{
    QT_TRACEOUT;

    try {
        assert(tableID);
        assert(mClientAuthorized);

        if(mGameStatus != GAM_OPPONENT_JOINED) {
            THROW_EXCEPTION("No opponent joined");
        }

        // send command
        QByteArray data = Q_BYTE_ARRAY(tableID);
        sendCmd(CHS, CMD_OPREJECT, data);
    }
    catch (Exception& e) {
        e.add(tr("Can't disagree to start game. Table ID ") + QString::number(tableID) + tr(" on server: ") + mSocket.peerName() + ". ");
        qDebug() << e.what();
        emit error (e.what());
    }
}

/*====================================================================================================
  __  ___  ____    ___  __  ___  __    ___
 / _)(  _)(_  _)  (  _)(  )(  _)(  )  (   \
( (/\ ) _)  )(     ) _) )(  ) _) )(__  ) ) )
 \__/(___) (__)   (_)  (__)(___)(____)(___/
====================================================================================================*/

void Client::getField (TABLEID tableID)
{
    QT_TRACEOUT;

    try {
        assert(tableID);
        assert(mClientAuthorized);

        // send command
        QByteArray data = Q_BYTE_ARRAY(tableID);
        sendCmd(CHS, CMD_GET_FIELD, data);
    }
    catch (Exception& e) {
        e.add(tr("Can't get field. Table ID ") + QString::number(tableID) + tr(" on server: ") + mSocket.peerName() + ". ");
        qDebug() << e.what();
        emit error (e.what());
    }

}

/*====================================================================================================
 ___  ____  ___  ___
/ __)(_  _)(  _)(  ,\
\__ \  )(   ) _) ) _/
(___/ (__) (___)(_)
====================================================================================================*/

void Client::move(TABLEID tableID, const Move& move, piece_type promotion)
{
    QT_TRACEOUT;
    assert(tableID);

    try {
        assert(mClientAuthorized);
        assert(mGameStatus == GAM_STARTED);

        // send command
        QByteArray data = Q_BYTE_ARRAY(tableID);
        data += Global::letter(move.srcCell);
        data += Global::number(move.srcCell);
        data += Global::letter(move.dstCell);
        data += Global::number(move.dstCell);

        switch(promotion)
        {
            case Queen:   data += 'q'; break;
            case Rook:    data += 'r'; break;
            case Knight:  data += 'n'; break;
            case Bishop:  data += 'b'; break;

            default: break;
        }

//        qDebug() << "Client::move data.size() " << data.size();

        sendCmd(CHS, CMD_MOVE, data);
    }
    catch (Exception& e) {
        e.add(tr("Can't get field. Table ID ") + QString::number(tableID) + tr(" on server: ") + mSocket.peerName() + ". ");
        emit error (e.what());
    }
}


/*====================================================================================================
 ___  _  _  ___   ___   ___  _  _  ___  ___  ___
/ __)( )( )(  ,) (  ,) (  _)( \( )(   \(  _)(  ,)
\__ \ )()(  )  \  )  \  ) _) )  (  ) ) )) _) )  \
(___/ \__/ (_)\_)(_)\_)(___)(_)\_)(___/(___)(_)\_)
====================================================================================================*/
void Client::surrender(TABLEID tableID)
{
    QT_TRACEOUT;

    try {
        assert(tableID);
        assert(mClientAuthorized);
        assert(mGameStatus == GAM_STARTED);

        // send command
        QByteArray data = Q_BYTE_ARRAY(tableID);
        sendCmd(CHS, CMD_SURRENDER, data);
    }
    catch (Exception& e) {
        e.add(tr("Can't surrender. Table ID ") + QString::number(tableID) + tr(" on server: ") + mSocket.peerName() + ". ");
        emit error (e.what());
    }
}

void Client::timeout (TABLEID tableID)
{
    QT_TRACEOUT;

    try {
        assert(tableID);
        assert(mClientAuthorized);
        assert(mGameStatus == GAM_STARTED);

        // send command
        QByteArray data = Q_BYTE_ARRAY(tableID);
        sendCmd(CHS, CMD_TIMEOUT, data);
    }
    catch (Exception& e) {
        e.add(tr("Can't say TIMEOUT. Table ID ") + QString::number(tableID) + tr(" on server: ") + mSocket.peerName() + ". ");
        emit error (e.what());
    }
}

/*
void Client::getTime(TABLEID tableID)
{
    QT_TRACEOUT;

    try {
        assert(tableID);
        assert(mClientAuthorized);
        assert(mGameStatus == GAM_STARTED);

        // send command
        QByteArray data = Q_BYTE_ARRAY(tableID);
        sendCmd(CHS, CMD_CHECK_TIME, data);
    }
    catch (Exception& e) {
        e.add(tr("Can't get time. Table ID ") + QString::number(tableID) + tr(" on server: ") + mSocket.peerName() + ". ");
        emit error (e.what());
    }
}
*/

void Client::getMyRating()
{
    QT_TRACEOUT;

    try {
        assert(mClientAuthorized);

        // send command
        QByteArray data;
        sendCmd(CHS, CMD_RATING, data);
    }
    catch (Exception& e) {
        e.add(tr("Can't get my rating ") + tr(" on server: ") + mSocket.peerName() + ". ");
        emit error (e.what());
    }
}

void Client::getLastGameResult()
{
    QT_TRACEOUT;

    try {
        assert(mClientAuthorized);

        // send command
        QByteArray data;
        sendCmd(CHS, CMD_LAST_GAME_RESULT, data);
    }
    catch (Exception& e) {
        e.add(tr("Can't get last game result ") + tr(" on server: ") + mSocket.peerName() + ". ");
        emit error (e.what());
    }
}

void Client::deleteLastGameResult()
{
    QT_TRACEOUT;

    try {
        assert(mClientAuthorized);

        // send command
        QByteArray data;
        sendCmd(CHS, CMD_DELETE_LAST_GAME_RESULT, data);
    }
    catch (Exception& e) {
        e.add(tr("Can't delete last game result ") + tr(" on server: ") + mSocket.peerName() + ". ");
        emit error (e.what());
    }
}



/*====================================================================================================
  __  ___  ___  ___  ___     ___  ___    __  _    _
 /  \(  _)(  _)(  _)(  ,)   (   \(  ,)  (  )( \/\/ )
( () )) _) ) _) ) _) )  \    ) ) ))  \  /__\ \    /
 \__/(_)  (_)  (___)(_)\_)  (___/(_)\_)(_)(_) \/\/
====================================================================================================*/
void Client::offerDraw(TABLEID tableID)
{
    QT_TRACEOUT;

    try {
        assert(tableID);
        assert(mClientAuthorized);
        assert(mGameStatus == GAM_STARTED);

        // send command
        QByteArray data = Q_BYTE_ARRAY(tableID);
        sendCmd(CHS, CMD_DRAW, data);
    }
    catch (Exception& e) {
        e.add(tr("Can't surrender. Table ID ") + QString::number(tableID) + tr(" on server: ") + mSocket.peerName() + ". ");
        emit error (e.what());
    }
}

/*====================================================================================================
 ___   ___  ___  __    _  _    ___  ___    __  _    _
(  ,) (  _)(  ,\(  )  ( \/ )  (   \(  ,)  (  )( \/\/ )
 )  \  ) _) ) _/ )(__  \  /    ) ) ))  \  /__\ \    /
(_)\_)(___)(_)  (____)(__/    (___/(_)\_)(_)(_) \/\/
====================================================================================================*/
void Client::replyDraw(TABLEID tableID, bool agree)
{
    QT_TRACEOUT;

    try {
        assert(tableID);
        assert(mClientAuthorized);
        assert(mGameStatus == GAM_STARTED);

        // send command
        char key = agree ? 'Y' : 'N';
        QByteArray data = Q_BYTE_ARRAY(tableID) + Q_BYTE_ARRAY(key);

        sendCmd(CHS, CMD_DRAGREE, data);
    }
    catch (Exception& e) {
        e.add(tr("Can't surrender. Table ID ") + QString::number(tableID) + tr(" on server: ") + mSocket.peerName() + ". ");
        emit error (e.what());
    }
}



void Client::joinChat (LOGICID logicID, TABLEID tableID)
{
    QT_TRACEOUT;
    try {
        assert(mClientAuthorized);

        // send command
        QByteArray data = Q_BYTE_ARRAY(logicID) + Q_BYTE_ARRAY(tableID);
        sendCmd(CHAT, CMD_CHAT_JOIN, data);

    }
    catch (Exception& e) {
        e.add(tr("Can't join chat. Table ID ") + QString::number(tableID) + tr(" on server: ") + mSocket.peerName() + ". ");
        emit error (e.what());
    }
}

void Client::leaveChat (LOGICID logicID)
{
    QT_TRACEOUT;
    try {
        assert(mClientAuthorized);

        // send command
        QByteArray data = Q_BYTE_ARRAY(logicID);
        sendCmd(CHAT, CMD_CHAT_LEAVE, data);

    }
    catch (Exception& e) {
        e.add(tr("Can't leave chat ")  + tr(" on server: ") + mSocket.peerName() + ". ");
        emit error (e.what());
    }

}

void Client::sendChatMessage(LOGICID logicID, TABLEID tableID, const QString& message)
{
    QT_TRACEOUT;
    try {
        assert(mClientAuthorized);

        // send command
        QByteArray data = Q_BYTE_ARRAY(logicID) + Q_BYTE_ARRAY(tableID) + message.toUtf8();
        sendCmd(CHAT, CMD_CHAT_MSG, data);

    }
    catch (Exception& e) {
        e.add(tr("Can't send table chat message. Table ID ") + QString::number(tableID) + tr(" on server: ") + mSocket.peerName() + ". ");
        emit error (e.what());
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
    QT_TRACEOUT;

    if(mSocket.state() != QAbstractSocket::ConnectedState)
    {
        return;
        //THROW_EXCEPTION(tr("Connection has been lost."));
    }

    char crc = 0;

    MessageHeader header;
    header.sign    = PROTOCOL_SIGNATURE;
    header.version = PROTOCOL_VERSION;
    header.service = service;
    header.cmd = command;

    QByteArray infPart;
    infPart += header.version;
    infPart += Q_BYTE_ARRAY(header.service);
    infPart += command;
    infPart += data;

    header.size = infPart.length() + sizeof(crc);

    QByteArray message = Q_BYTE_ARRAY(header);
    message += data;
    message += Global::getCRC(infPart);

    qint64 bytes = mSocket.write(message);
    if(bytes == -1) {
        THROW_EXCEPTION(tr("Can't send command with ID: ") + QString::number(command) + ".");
    }

    Q_ASSERT(bytes == message.size());

QString str = Global::timestamp() + " OUTG: " +
              Global::serviceToString(service) + " " +
              GlobalServer::commandToString(command).c_str() + "; ";
for(int i=0; i<message.size(); i++)
    str += QString::number((int)(unsigned char)message[i]) + " ";
qDebug() << str;

}




/*====================================================================================================
  __  _  _    ___   ___   __   ___  _  _    ___   ___   __   ___
 /  \( \( )  (  ,) (  _) (  ) (   \( \/ )  (  ,) (  _) (  ) (   \
( () ))  (    )  \  ) _) /__\  ) ) )\  /    )  \  ) _) /__\  ) ) )
 \__/(_)\_)  (_)\_)(___)(_)(_)(___/(__/    (_)\_)(___)(_)(_)(___/
====================================================================================================*/


void Client::onReadyRead()
{
    QT_TRACEOUT;

    if(mSocket.state() != QAbstractSocket::ConnectedState) {
        qDebug("No connection to server");
        return;
    }

    QByteArray buf = mSocket.readAll();



    // >>> goto here
    while(buf.size() > 0)
    {

        MessageHeader* header = (MessageHeader*)buf.data();


        if(header->sign != PROTOCOL_SIGNATURE) {
            qDebug("Server uses wrong protocol ");
            return;
        }

        unsigned mesSize = sizeof(header->sign) + sizeof(header->size) + header->size;

QString str = Global::timestamp() + "  INC: " +
              Global::serviceToString((quint32)header->service) + " " +
              GlobalServer::commandToString((quint32)header->cmd).c_str() + "; ";
for(int i=0; i<mesSize; i++)
    str += QString::number((int)(unsigned char)buf[i]) + " ";
qDebug() << str;


        QByteArray infPart((char*)&header->version, header->size - CRC_SIZE);
        if(buf[mesSize - CRC_SIZE] != Global::getCRC(infPart)) {
            qDebug() << "Server response has bad CRC";
            return;
        }

        if(header->version != PROTOCOL_VERSION) {
            qDebug("Server uses another protocol, version: ");// + QString::number(header->version) +
                  //  " (expected " + QString::number(PROTOCOL_VERSION) + ").");
            return;
        }

        // save message in the pool

        char* dataOffset = buf.data() + sizeof(MessageHeader);
        unsigned dataSize = header->size - sizeof(header->version) - sizeof(header->service) - sizeof(header->cmd) - CRC_SIZE;

        QByteArray message(dataOffset, dataSize);



        switch(header->service)
        {
            case SRV:   processMessageSRV (*header, message); break;
            case TBM:   processMessageTBM (*header, message); break;
            case CHS:   processMessageCHS (*header, message); break;
            case REG:   processMessageREG (*header, message); break;
            case CHAT:  processMessageCHAT(*header, message); break;
            default:    THROW_EXCEPTION(tr("Logic error"));
        }

        // The buffer probably has got another message
        QByteArray newbuf(buf.data() + mesSize, buf.size() - mesSize);
        buf = newbuf;
    }
}

/*====================================================================================================
 ___  ___   __    __  ___  ___  ___    __  __  ___  ___  ___   __   __  ___    ___  ___  _  _
(  ,\(  ,) /  \  / _)(  _)/ __)/ __)  (  \/  )(  _)/ __)/ __) (  ) / _)(  _)  / __)(  ,)( )( )
 ) _/ )  \( () )( (_  ) _)\__ \\__ \   )    (  ) _)\__ \\__ \ /__\( (/\ ) _)  \__ \ )  \ \\//
(_)  (_)\_)\__/  \__)(___)(___/(___/  (_/\/\_)(___)(___/(___/(_)(_)\__/(___)  (___/(_)\_)(__)
====================================================================================================*/
void Client::processMessageSRV(const MessageHeader& header, const QByteArray& buffer)
{
 //   qDebug() << "Client::processMessageSRV: buffer[0]=" << (int)buffer[0] <<  "  cmd =" << (int)header.cmd;
    // LOGIN
    if(header.cmd == LOGIN_STATUS)
    {
        switch(buffer[0]) {
            case NOERR:          mClientAuthorized = true; emit authorized(); break;
            case ERRUSERONLINE:  emit notAuthorized(tr("The user is already online.")); break;
            case ERRBADLOGIN:    emit notAuthorized(tr("Incorrect user name.")); break;
            case ERRBADPASS:     emit notAuthorized(tr("Incorrect password.")); break;
            default:             emit notAuthorized(tr("Internal server error ") + QString::number(buffer[0]) + "."); break;
        }
    }
    else
    {
        struct Reply {
            char        err;
        };

        Reply* reply = (Reply*)buffer.data();
        emit error(tr("SRV Error: ") + QString::number((int)reply->err));
    }
}

/*====================================================================================================
 ___  ___   __    __  ___  ___  ___    __  __  ___  ___  ___   __   __  ___    ___   ___   __
(  ,\(  ,) /  \  / _)(  _)/ __)/ __)  (  \/  )(  _)/ __)/ __) (  ) / _)(  _)  (  ,) (  _) / _)
 ) _/ )  \( () )( (_  ) _)\__ \\__ \   )    (  ) _)\__ \\__ \ /__\( (/\ ) _)   )  \  ) _)( (/\
(_)  (_)\_)\__/  \__)(___)(___/(___/  (_/\/\_)(___)(___/(___/(_)(_)\__/(___)  (_)\_)(___) \__/
====================================================================================================*/
void Client::processMessageREG(const MessageHeader& header, const QByteArray& buffer)
{
    // REGISTER
    if(header.cmd == REG_STATUS)
    {
        switch(buffer[0]) {
            case NOERR:          emit registered(); break;
            case ERRBADLOGIN:    emit error(tr("Incorrect user name")); break;
            case ERRBADPASS:     emit error(tr("Incorrect password")); break;
            case ERRLOGINEXIST:  emit error(tr("User already exists.")); break;
            default:             emit error(tr("Internal server error ") + QString::number(buffer[0]) + "."); break;
        }
    }
    else
    {
        struct Reply {
            char        err;
        };

        Reply* reply = (Reply*)buffer.data();
        emit error(tr("REG Error: ") + QString::number((int)reply->err));
    }
}

/*====================================================================================================
 ___  ___   __    __  ___  ___  ___    __  __  ___  ___  ___   __   __  ___    ____  ___  __  __
(  ,\(  ,) /  \  / _)(  _)/ __)/ __)  (  \/  )(  _)/ __)/ __) (  ) / _)(  _)  (_  _)(  ,)(  \/  )
 ) _/ )  \( () )( (_  ) _)\__ \\__ \   )    (  ) _)\__ \\__ \ /__\( (/\ ) _)    )(   ) ,\ )    (
(_)  (_)\_)\__/  \__)(___)(___/(___/  (_/\/\_)(___)(___/(___/(_)(_)\__/(___)   (__) (___/(_/\/\_)
====================================================================================================*/
void Client::processMessageTBM(const MessageHeader& header, const QByteArray& buffer)
{
    // CREATE GAME TABLE
    if(header.cmd == ANS_CREATE)
    {
        struct Reply {
            TABLEID     tableID;
            char        isValid;
        };

        Reply* reply = (Reply*)buffer.data();

        switch(reply->isValid)
        {
            case ST_VALID:       emit gameTableCreated(reply->tableID); qDebug() << "Table ID" << reply->tableID; break;
            case ST_NOTVALID:    emit error(tr("Invalid parameter or table already exists")); break; // TODO What parameter is wrong (exactly)?
            default:             emit error(tr("Internal server error") + QString::number(reply->isValid)); break;
        }
    }
    // DELETE GAME TABLE
    else if(header.cmd == ANS_DELETE)
    {
        // get server reply
        struct Reply {
            LOGICID     logicID;
            TABLEID     tableID;
            char        isValid;
        };

        Reply* reply = (Reply*)buffer.data();

        switch(reply->isValid) {
            case ST_VALID:
            case ST_NOTVALID:    emit gameTableDeleted(); break; //emit error(tr("Invalid parameter. Attempt to delete wrong Game Table?"));  break;
            default:             emit error(tr("Internal server error") + QString::number(reply->isValid)); break;
        }
    }
    else if(header.cmd == ANS_TABLE)
    {
        // get server reply
        struct Reply {
            LOGICID     logicID;
            quint32     count;
        };

        Reply* reply = (Reply*)buffer.data();

        QList<GameTable> tables;
        for(quint32 i=0; i < reply->count; i++)
        {
            GameTable table;
            TABLEID* id = (TABLEID*)(buffer.data() + sizeof(Reply) + i*sizeof(TABLEID));
            table.id = *id;

            tables << table;
        }

        emit gotGameTables(tables);

   //     qDebug() << "reply->count" << reply->count ;
   //     qDebug() << "OK: findGameTables." ;
    }
    // GET MY GAME TABLE

    else if(header.cmd == ANS_MYTBL)
    {
   //     qDebug() << "cmd == ANS_MYTBL" ;
        struct Reply {
            TABLEID     tableID;
            char        isValid;
        };
        Reply* reply = (Reply*)buffer.data();

        switch(reply->isValid) {
            case ST_VALID:            emit gotMyGameTable(reply->tableID, false);  break;
            case ST_VALID_AND_OWNER:  emit gotMyGameTable(reply->tableID, true);  break;
            case ST_NOTVALID:         emit gotMyGameTable(0, false);  break;
            default:                  emit error(tr("Internal server error") + QString::number(reply->isValid)); break;
        }
    }

    // GET RANDOM GAME TABLE

    else if(header.cmd == ANS_RANDOM_OP)
    {
        // get server reply
        struct Reply {
            LOGICID     logicID;
            TABLEID     tableID;
        };

        Reply* reply = (Reply*)buffer.data();

        QList<GameTable> tables;

        GameTable table;
        table.id = reply->tableID;
        tables << table;

        emit gotGameTables(tables);

    }

    else if(header.cmd == ANS_GET_PARAMS)
    {
        // get server reply
        struct Reply {
            TABLEID     tableID;
            char        isValid;
            qint32      gameTimeID;
            qint32      gameTime;
            qint32      moveTimeID;
            qint32      moveTime;
            qint32      playerNameID;
        };

        Reply* reply = (Reply*)buffer.data();
        QString name = "";

        if(reply->isValid == ST_NOTVALID)
        {
            return;
        }

        assert(reply->moveTimeID == PARAMETER_ID_MOVETIME);
        assert(reply->gameTimeID == PARAMETER_ID_GAMETIME);
        assert(reply->playerNameID == PARAMETER_ID_PLAYER_NAME);


        for(char* c = (char*)(buffer.data() + sizeof(Reply)); *c != '\0'; c++)
        {
            name += *c;
        }

        int* rating = (int*)(buffer.data() + sizeof(Reply) + name.size() + sizeof('\0'));

        GameTable table;
        table.id = reply->tableID;
        table.host.name = name;
        table.host.rating = *rating;
        table.time2step = reply->moveTime;
        table.time2game = reply->gameTime;

        emit gotGameTableParams(table);



    }
    else
    {
        struct Reply {
            char        err;
        };

        Reply* reply = (Reply*)buffer.data();
        emit error(tr("TBM Error: ") + QString::number((int)reply->err));
    }
}

/*====================================================================================================
 ___  ___   __    __  ___  ___  ___    __  __  ___  ___  ___   __   __  ___     __  _  _  ___
(  ,\(  ,) /  \  / _)(  _)/ __)/ __)  (  \/  )(  _)/ __)/ __) (  ) / _)(  _)   / _)( )( )/ __)
 ) _/ )  \( () )( (_  ) _)\__ \\__ \   )    (  ) _)\__ \\__ \ /__\( (/\ ) _)  ( (_  )__( \__ \
(_)  (_)\_)\__/  \__)(___)(___/(___/  (_/\/\_)(___)(___/(___/(_)(_)\__/(___)   \__)(_)(_)(___/
====================================================================================================*/
void Client::processMessageCHS(const MessageHeader& header, const QByteArray& buffer)
{
    // OPPONENT CONNECTED
    if(header.cmd == ANS_OPPONENT_JOINED)
    {
        struct Reply {
            TABLEID     tableID;
            PLAYERID    opponentID;
            quint32     rating;
        };

        Reply* reply = (Reply*)buffer.data();

        // Opponent name goes after the struct 'Reply'
        QString opponentName = "";
        opponentName.append(QByteArray(buffer.data() + sizeof(Reply), buffer.size() - sizeof(Reply)));

        mGameStatus = GAM_OPPONENT_JOINED;
        qDebug() << mName << "emit opponentJoined " << reply->opponentID;
        emit opponentJoined(opponentName, reply->rating);
    }
    else if(header.cmd == ANS_GET_OPPONENT)
    {
        struct Reply {
            TABLEID     tableID;
            PLAYERID    opponentID;
            quint32     rating;
        };

        Reply* reply = (Reply*)buffer.data();

        // Opponent name goes after the struct 'Reply'
        QString opponentName = "";
        opponentName.append(QByteArray(buffer.data() + sizeof(Reply), buffer.size() - sizeof(Reply)));

        mGameStatus = GAM_OPPONENT_JOINED;
        //qDebug() << mName << "GAM_OPPONENT_JOINED " << reply->opponentID;
        emit gotOpponent(opponentName, reply->rating);
    }

    // GAME STARTED
    else if(header.cmd == ANS_START)
    {
        mGameStatus = GAM_STARTED;
        //qDebug() << mName << "GAM_STARTED";
        emit gameStarted();
    }

    // GET FIELD
    else if(header.cmd == ANS_FIELD)
    {
        struct Reply {
            TABLEID     tableID;
            char        playerNr; // table owner (?)
            uint32_t    moveTime;
            uint32_t    gameTime;
            char        cells[CELLS_IN_FIELD];
            char        whitePlayerNr;
            char        myMove;
        };

        Reply* reply = (Reply*)buffer.data();

/*        qDebug() << "playerNr " << (int)reply->playerNr  <<
                    " whitePlayerNr " << (int)reply->whitePlayerNr <<
                    " myMove "<< (int)reply->myMove;*/

        Field field;
        for(int i = 0; i<CELLS_IN_FIELD; ++i)
            field.push_back((piece_type)reply->cells[i]);


        emit gotField(field, reply->myMove, reply->playerNr == reply->whitePlayerNr);
        emit gotMoveTime(reply->moveTime);
        emit gotGameTime(reply->gameTime);
    }
    // STEP
    else if(header.cmd == ANS_MOVE)
    {
        struct Reply {
            TABLEID     tableID;
            char        isValid;
        };

        Reply* reply = (Reply*)buffer.data();

        switch(reply->isValid) {
            case P_DONE:         break;
            case P_NOT_VALID:    emit invalidMove();  break;
            default:             emit error(tr("Internal server error") + QString::number(reply->isValid)); break;
        }

    }
    // JOIN
    else if(header.cmd == ANS_JOIN)
    {
        // get server reply
        struct Reply {
            TABLEID     tableID;
            char        status;
        };

        Reply* reply = (Reply*)buffer.data();

        switch(reply->status) {
            case P_DONE:       emit joined(reply->tableID); break;
            case P_FAILED:     emit error(tr("Can't join to the game table."));  break;
            default:           emit error(tr("Internal server error ") + QString::number(reply->status)); break;
        }
    }
    // DISAGREE TO START A GAME
    else if(header.cmd == ANS_OPREJECT)
    {
        // get server reply
        struct Reply {
            TABLEID     tableID;
            char        status;
        };

        Reply* reply = (Reply*)buffer.data();

        switch(reply->status) {
            case P_DONE:       emit gameRejected(); break;
            case P_FAILED:     emit error(tr("Can't reject the game."));  break;
            default:           emit error(tr("Internal server error ") + QString::number(reply->status)); break;
        }
    }
    // GAME OVER
    else if(header.cmd == ANS_END)
    {
        struct Reply {
            TABLEID     tableID;
            char        status;
            quint32     rating;
        };

        Reply* reply = (Reply*)buffer.data();

        QString text = tr("Game over.") + "\n\n" + Global::getGameResultText(reply->status, reply->rating);


        emit gotMyRating(reply->rating);
        emit gameOver(text);
    }

    // DRAW
    else if(header.cmd == ANS_DRAW)
    {
        struct Reply {
            TABLEID     tableID;
            char        status;
        };

        Reply* reply = (Reply*)buffer.data();

        switch(reply->status) {
            case P_OFFER:      emit drawOffered(); break;
            case P_WAIT:        break; // TODO send a message "Please wait"
            case P_ACCEPT:     emit gameOver(tr("A draw.")); break;
            case P_REJECT:     emit drawRejected(tr("Your opponent has rejected the draw")); break;
            case P_NOTALLOWED: emit drawRejected(tr("Please wait for your next move to offer a draw")); break;
            default:           emit error(tr("Internal server error ") + QString::number(reply->status)); break;
        }
    }

    // TIME

    else if(header.cmd == ANS_CHECK_TIME_STEP)
    {
        struct Reply {
            TABLEID     tableID;
            qint32      time2step;
        };

        Reply* reply = (Reply*)buffer.data();

        emit gotMoveTime(reply->time2step);
      //  qDebug() << "tableID: " << reply->tableID << " time2step: " << reply->time2step;
    }
    else if(header.cmd == ANS_CHECK_TIME_GAME)
    {
        struct Reply {
            TABLEID     tableID;
            qint32      time2game;
        };

        Reply* reply = (Reply*)buffer.data();

        emit gotGameTime(reply->time2game);
       // qDebug() << "time2game: " << reply->time2game;
    }
    else if(header.cmd == ANS_RATING)
    {
        struct Reply {
            TABLEID     tableID;
            qint32      rating;
        };

        Reply* reply = (Reply*)buffer.data();

        emit gotMyRating(reply->rating);
       // qDebug() << "time2game: " << reply->time2game;
    }
    else if(header.cmd == ANS_LAST_GAME_RESULT)
    {
        struct Reply {
            TABLEID     tableID;
            qint32      result;
        };

        Reply* reply = (Reply*)buffer.data();

        emit gotLastGameResult(reply->result);
       // qDebug() << "time2game: " << reply->time2game;
    }
    else
    {
        struct Reply {
            char        err;
        };

        QString str = Global::timestamp() + "  ERR: " + "\n";

        str += "header.cmd: " + QString::number((int)(unsigned char)header.cmd) + "\n";
        str += "header.service: " + QString::number(header.service) + "\n";
        str += "header.sign: " + QString::number((int)(unsigned char)header.sign) + "\n";
        str += "header.size: " + QString::number(header.size) + "\n";
        str += "header.version: " + QString::number((int)(unsigned char)header.version) + "\n";

        for(int i=0; i<buffer.size(); i++)
            str += QString::number((int)(unsigned char)buffer[i]) + " ";
        str += "\n ANS_OPPONENT_JOINED : " + QString::number(ANS_OPPONENT_JOINED) + "\n";
        qDebug() << str;

        Reply* reply = (Reply*)buffer.data();
        emit error(tr("CHS Error: ") + QString::number((int)reply->err));
    }
}


void Client::processMessageCHAT(const MessageHeader& header, const QByteArray& buffer)
{
    if(header.cmd == ANS_CHAT_MSG)
    {
        emit chatMessage(QString::fromUtf8(buffer.data()));
    }
    else if(header.cmd == ANS_CHAT_USER_ONLINE)
    {
        emit chatUserOnline(QString::fromUtf8(buffer.data()));
    }
    else if(header.cmd == ANS_CHAT_USER_JOINED)
    {
        emit chatUserJoined(QString::fromUtf8(buffer.data()));
    }
    else if(header.cmd == ANS_CHAT_USER_LEFT)
    {
        emit chatUserLeft(QString::fromUtf8(buffer.data()));
    }
    else
    {
        struct Reply {
            char        err;
        };

        Reply* reply = (Reply*)buffer.data();
        emit error(tr("CHAT Error: ") + QString::number((int)reply->err));
    }

}


/*====================================================================================================
  __  _  _     __  __  _  _  _  _  ___   __  ____  ___  ___
 /  \( \( )   / _)/  \( \( )( \( )(  _) / _)(_  _)(  _)(   \
( () ))  (   ( (_( () ))  (  )  (  ) _)( (_   )(   ) _) ) ) )
 \__/(_)\_)   \__)\__/(_)\_)(_)\_)(___) \__) (__) (___)(___/
====================================================================================================*/
void Client::onConnected()
{
    QT_TRACEOUT;

    delete mTimer;
    //qDebug() << "SIGNAL connected";
    emit connectedToHost();
}

void Client::onDisconnected()
{
    QT_TRACEOUT;

    //qDebug() << "SIGNAL disconnected";
    emit disconnectedFromHost();
}

/*====================================================================================================
  __  _  _    ___  ___   ___   __  ___
 /  \( \( )  (  _)(  ,) (  ,) /  \(  ,)
( () ))  (    ) _) )  \  )  \( () ))  \
 \__/(_)\_)  (___)(_)\_)(_)\_)\__/(_)\_)
====================================================================================================*/
void Client::onError()
{
    QString text = mSocket.errorString();

    if(text == "Connection refused")
    {
        text = tr("Connection refused. \n\nPlease check whether you are connected to Internet. "
                  "Check the server name and port as well.");
    }

    //qDebug() << "Client::onError: " << mSocket.errorString();
    emit error(text);
}

/*====================================================================================================
  __  _  _    ____  __  __  __  ___   __  _  _  ____
 /  \( \( )  (_  _)(  )(  \/  )(  _) /  \( )( )(_  _)
( () ))  (     )(   )(  )    (  ) _)( () ))()(   )(
 \__/(_)\_)   (__) (__)(_/\/\_)(___) \__/ \__/  (__)
====================================================================================================*/
void Client::onTimeout()
{
    delete mTimer;
    mSocket.abort();

    //qDebug() << "onTimeout";
    emit error(tr("Server doesn't respond"));
}


