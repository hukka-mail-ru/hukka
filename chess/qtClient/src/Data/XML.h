#ifndef XML_CLASS_H_
#define XML_CLASS_H_

#include <QString>
#include <QList>
#include <QtXml/QDomNode>


#define XML_CONFIG_FILENAME     QString("config/config.xml")
#define XML_ITEMS_FILENAME      ":/config/items.xml"
#define XML_FILE_TAG_IDENT      2

// CONFIG.XML ////////////////////////////////////////////
// 1st level
#define XML_NODE_SERVER     "server"
#define XML_NODE_CLIENT     "client"
#define XML_NODE_USER       "user"

// 2nd level
#define XML_NODE_NAME       "name"
#define XML_NODE_PORT       "port"
#define XML_NODE_LOGIN      "login"
#define XML_NODE_PASSWORD   "password"
#define XML_NODE_LANGUAGE   "language"

///////////////////////////////////////////////////////////

// ITEMS.XML  //////////////////////////////////////////
// 1st level
#define XML_NODE_MAIN_WINDOW      "MainWindow"
#define XML_NODE_MAIN_MENU        "MainMenu"
#define XML_NODE_SPLASH           "Splash"
#define XML_NODE_SCENE            "Scene"
#define XML_NODE_DIALOGS          "Dialogs"
#define XML_NODE_BUTTONS          "Buttons"
#define XML_NODE_PROMOTION        "Promotion"
#define XML_NODE_EXIT             "Exit"
#define XML_NODE_COMMON_CHAT      "CommonChat"
#define XML_NODE_TABLE_CHAT       "TableChat"
#define XML_NODE_CELL             "Cell"
#define XML_NODE_PIECE            "Piece"
#define XML_NODE_PAWN             "Pawn"
#define XML_NODE_CAPTURE_BOX      "CaptureBox"
#define XML_NODE_MOVE_BOX         "MoveBox"
#define XML_NODE_BOARD            "Board"
#define XML_NODE_ANIMATION        "Animation"
#define XML_NODE_GAME_STATE       "GameState"

// Buttons
#define XML_NODE_NEW_GAME         "NewGame"
#define XML_NODE_FIND_GAME        "FindGame"
#define XML_NODE_CHAT             "Chat"
#define XML_NODE_WALLET           "Wallet"
#define XML_NODE_OPTIONS          "Options"
#define XML_NODE_GAME_MENU        "GameMenu"

// Clocks
#define XML_NODE_MOVE_CLOCK       "MoveClock"
#define XML_NODE_GAME_CLOCK       "GameClock"


// 2nd level
#define XML_NODE_BORDER      "border"
#define XML_NODE_X           "x"
#define XML_NODE_Y           "y"
#define XML_NODE_WIDTH       "width"
#define XML_NODE_HEIGHT      "height"
#define XML_NODE_TEXT_OFFSET "textOffset"
#define XML_NODE_MARGIN      "margin"

#define XML_NODE_STYLE       "style"
#define XML_NODE_FONT        "font"
#define XML_NODE_FAMILY      "family"
#define XML_NODE_SIZE        "size"
#define XML_NODE_COLOR       "color"
#define XML_NODE_ACTIVE      "active"
#define XML_NODE_INACTIVE    "inactive"

#define XML_NODE_PLAYER      "player"
#define XML_NODE_ME          "me"
#define XML_NODE_OPPONENT    "opponent"
#define XML_NODE_RATING      "rating"
#define XML_NODE_BALANCE     "balance"

#define XML_NODE_TIMER       "timer"

#define XML_VALUE_TAG        "value"


#define XML_NODE_HISTORY     "history"
#define XML_NODE_USERLIST    "userlist"
#define XML_NODE_HEADER		 "header"

class XML
{
    XML() {}

public:

    static XML& instance()
    {
        static XML xml;
        return xml;
    }

    QString readValue(const QString& filename, const QList<QString>& nodenames);
    void writeValue(const QString& filename, const QList<QString>& nodenames, const QString& value);

private:

    QDomDocument mConfig;
};


#endif
