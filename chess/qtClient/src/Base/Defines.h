/*
 * Defines.h
 *
 *  Created on: May 19, 2010
 *      Author: ssy
 */

#ifndef DEFINES_H_
#define DEFINES_H_

#include <QObject>
#define LANGUAGE_ENGLISH                "0"
#define LANGUAGE_RUSSIAN                "1"

#define MINUTES_IN_HOUR                 60
#define SECONDS_IN_MINUTE               60
#define SECONDS_IN_HOUR                 3600

#define WAIT_CONNECT_TIMEOUT            3 // seconds
#define WAIT_RESPONSE_TIMEOUT           10000 // milliseconds
#define MESSAGE_POOL_QUERY_DELAY        1000  // milliseconds

#define PROTOCOL_SIGNATURE              'Z'
#define PROTOCOL_VERSION                2

#define CRC_SIZE                        sizeof(char)

// Connect
#define DEFAULT_PROXY                   QNetworkProxy(QNetworkProxy::HttpProxy)

// services (from wsUsers table)
#define SRV                             1
#define REG                             2
#define CHAT                            3
#define TBM                             4 // Table Manager
#define CHS                             6 // Chess Server


// server commands
#define CMD_LOGIN                       1
#define CMD_REG                         1

// server replies
#define LOGIN_STATUS                    1
#define REG_STATUS                      1

// game logic ids (see tbLogicList table)
#define LOGIC_ID_GAMMON                 1
#define LOGIC_ID_CHESS                  2

// game table parameter ids (see tbParameterList table)
#define PARAMETER_ID_PLAYER_NAME        0
#define PARAMETER_ID_OPPONENT_NAME      1
#define PARAMETER_ID_PASSWD             2
#define PARAMETER_ID_MOVETIME           3
#define PARAMETER_ID_GAMETIME           4
#define PARAMETER_ID_MINRATING          5
#define PARAMETER_ID_MAXRATING          6

// game table parameter constrains (see tbParamList table)
#define DEFAULT_MOVETIME                7 // minutes
#define DEFAULT_GAMETIME                120 // minutes
#define DEFAULT_MINRATING               0
#define DEFAULT_MAXRATING               0 // TODO if set 500 the server reports an error!
#define DEFAULT_MAXCOUNT                0xFFFFFFFF

// comparision operators in CMD_FIND-like commands
#define OPERATOR_MORE                   0
#define OPERATOR_EQUAL                  1
#define OPERATOR_LESS                   2

// logical operators in CMD_FIND-like commands
#define OPERATOR_OR                     1
#define OPERATOR_AND                    2
#define OPERATOR_LAST                   0

#define Q_BYTE_ARRAY(var)        QByteArray((char*)&var, sizeof(var))


typedef quint32 PARAMID;
typedef quint32 GameTableStatus;

#define ID_NOT_DEFINED                  0xFFFFFFFF

/////////////////////////////////////////////////////////////////////////////////////////


#define  OPAQUE_NORMAL                  1.0
#define  OPAQUE_HALF                    0.3


#define CELLS_IN_FIELD                  64 // items
#define CELLS_IN_ROW                    8  // items


// Z-layers of the Scene. 0 is the lowest layer
#define Z_MESSAGE_LAYER                 100
#define Z_DIALOG_LAYER                  20
#define Z_TEXT_LAYER                    6
#define Z_BUTTONS_LAYER                 5
#define Z_SPLASH_LAYER                  4
#define Z_NOTES_LAYER                   3
#define Z_PIECES_LAYER                  2
#define Z_HIGHLIGHT_LAYER               1
#define Z_CELLS_LAYER                   0



typedef quint32             CELLID;
typedef quint32             TABLEID;
typedef quint32             LOGICID;

enum GameState
{
    GS_WAIT_FOR_PLAYER_TOUCH,
    GS_WAIT_FOR_PLAYER_MOVE,
    GS_WAIT_FOR_SERVER,
    GS_WAIT_FOR_OPPONENT,
    GS_INVALID_MOVE
};

struct Move
{
    CELLID srcCell;
    CELLID dstCell;
};

enum PlayerColor
{
    PC_WHITE,
    PC_BLACK
};

enum ChatType
{
    CT_COMMON_CHAT,
    CT_TABLE_CHAT,
};


class Game
{
public:
    static char letter(CELLID cell) { return 'a' + cell % CELLS_IN_ROW; }
    static char number(CELLID cell) { return '1' + cell / CELLS_IN_ROW; }

    static QString seconds2hrs (quint32 seconds)
    {
        quint32 hrs = seconds / SECONDS_IN_HOUR;
        quint32 mins = (seconds - hrs * SECONDS_IN_HOUR) / SECONDS_IN_MINUTE;
        quint32 secs = seconds - hrs * SECONDS_IN_HOUR - mins * SECONDS_IN_MINUTE;

        QChar fill = QLatin1Char('0');

        return QString("%1:%2:%3")
                .arg(hrs, 1, 10, fill)
                .arg(mins, 2, 10, fill)
                .arg(secs, 2, 10, fill);
    }
};

#endif /* DEFINES_H_ */
