/*
 * Defines.h
 *
 *  Created on: May 19, 2010
 *      Author: ssy
 */

#ifndef DEFINES_H_
#define DEFINES_H_

#include <QObject>
#include <QTime>

#include <ChessTypes.h>
#include <header/defserver.h>
#include <header/defservice.h>

#define LANGUAGE_ENGLISH                "0"
#define LANGUAGE_RUSSIAN                "1"

const int MINUTES_IN_HOUR                = 60;
const int SECONDS_IN_MINUTE              = 60;
const int SECONDS_IN_HOUR                = 3600;

const int WAIT_CONNECT_TIMEOUT           = 3; // seconds
const int WAIT_RESPONSE_TIMEOUT          = 10000; // milliseconds
const int MESSAGE_POOL_QUERY_DELAY       = 1000;  // milliseconds

#define PROTOCOL_SIGNATURE              'Z'
const int PROTOCOL_VERSION               = 2;

#define CRC_SIZE                        sizeof(char)

// Connect
#define DEFAULT_PROXY                   QNetworkProxy(QNetworkProxy::HttpProxy)




// game table parameter ids (see tbParameterList table)
const int  PARAMETER_ID_PLAYER_NAME      =  0;
const int  PARAMETER_ID_OPPONENT_NAME    =  1;
const int  PARAMETER_ID_PASSWD           =  2;
const int  PARAMETER_ID_MOVETIME         =  3;
const int  PARAMETER_ID_GAMETIME         =  4;
const int  PARAMETER_ID_MINRATING        =  5;
const int  PARAMETER_ID_MAXRATING        =  6;

// game table parameter constrains (see tbParamList table)
const int  DEFAULT_MOVETIME               = 7; // minutes
const int  DEFAULT_GAMETIME               = 120; // minutes
const int  DEFAULT_MINRATING              = 0;
const int  DEFAULT_MAXRATING              = 0; // TODO if set 500 the server reports an error!
const int  DEFAULT_MAXCOUNT               = 0xFFFFFFFF;

// comparision operators in CMD_FIND-like commands
const int  OPERATOR_MORE                  = 0;
const int  OPERATOR_EQUAL                 = 1;
const int  OPERATOR_LESS                  = 2;

// logical operators in CMD_FIND-like commands
const int  OPERATOR_OR                    = 1;
const int  OPERATOR_AND                   = 2;
const int  OPERATOR_LAST                  = 0;

#define Q_BYTE_ARRAY(var)        QByteArray((char*)&var, sizeof(var))


typedef quint32 PARAMID;
typedef quint32 GameTableStatus;

const int  ID_NOT_DEFINED                 = 0xFFFFFFFF;

/////////////////////////////////////////////////////////////////////////////////////////


const double   OPAQUE_NORMAL                 = 1.0;
const double   OPAQUE_HALF                   = 0.3;


const int  CELLS_IN_FIELD                 = 64; // items
const int  CELLS_IN_ROW                   = 8 ; // items


// Z-layers of the Scene. 0 is the lowest layer
const int  Z_MESSAGE_LAYER                = 100;
const int  Z_DIALOG_LAYER                 = 90;
const int  Z_TEXT_LAYER                   = 80;
const int  Z_BUTTONS_LAYER                = 70;

const int  Z_UNDER_BUTTONS_LAYER                = 65;

const int  Z_SPLASH_LAYER                 = 60;
const int  Z_NOTES_LAYER                  = 50;
const int  Z_PIECES_LAYER                 = 40;
const int  Z_HIGHLIGHT_LAYER              = 30;
const int  Z_CELLS_LAYER                  = 0;

// Chat
#define ADDRESSEE_ALL tr("all")

typedef quint32             CELLID;
typedef quint32             TABLEID;
typedef quint32             LOGICID;
typedef quint32             PLAYERID;

typedef vector<piece_type>  Field;


enum GameState
{
    GS_WAIT_FOR_PLAYER_TOUCH,
    GS_WAIT_FOR_PLAYER_MOVE,
    GS_WAIT_FOR_SERVER,
    GS_WAIT_FOR_OPPONENT,
    GS_INVALID_MOVE,
    GS_GAME_OVER
};

enum PlayerColor
{
    PC_WHITE,
    PC_BLACK
};

enum HighlightColor
{
    HC_WHITE,
    HC_GRAY
};


enum PlayerType
{
    PT_ME,
    PT_OPPONENT
};


enum ChatType
{
    CT_COMMON_CHAT,
    CT_TABLE_CHAT,
};

struct Move
{
    CELLID srcCell;
    CELLID dstCell;
};

struct Position
{
    Field field;
    Move move;
    bool myMove;
    bool iAmWhite;
    bool w_check;
    bool b_check;
    ChessGameStatus status;
};

struct Player
{
    Player(): name(QObject::tr("offline")), rating(RATING_NOT_AVAILABLE), isAuthorized(false) {}

    QString     name;
    unsigned    rating;
    PlayerColor color;
    bool        isAuthorized;
};

struct GameTable
{
    TABLEID id;
    Player host;
    qint32 time2step;
    qint32 time2game;
};



#endif /* DEFINES_H_ */
