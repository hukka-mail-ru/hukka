#include <Chat.h>
#include <QScrollArea>
#include <QScrollBar>
#include <QVBoxLayout>
#include <QHBoxLayout>
#include <QWidget>
#include <Client.h>
#include <UI.h>
#include <MainWindow.h>
#include <XML.h>
#include <chatserver/chatdefs.h>


Chat::Chat(QWidget* parent, ChatType type):
    QDialog(parent), mChatType(type)
{
    // No title, no [X] button, no bottom
    setWindowFlags(Qt::Widget);

    QString chatNode = (mChatType == CT_COMMON_CHAT) ? XML_NODE_COMMON_CHAT : XML_NODE_TABLE_CHAT;

    QString style =  XML::instance().readValue(XML_ITEMS_FILENAME, QList<QString>() << chatNode << XML_NODE_STYLE);
    setStyleSheet(style);

    mColorMe       = XML::instance().readValue(XML_ITEMS_FILENAME, QList<QString>() << chatNode << XML_NODE_FONT << XML_NODE_ME << XML_NODE_COLOR);
    mColorOpponent = XML::instance().readValue(XML_ITEMS_FILENAME, QList<QString>() << chatNode << XML_NODE_FONT << XML_NODE_OPPONENT << XML_NODE_COLOR);
    mColorServer   = XML::instance().readValue(XML_ITEMS_FILENAME, QList<QString>() << chatNode << XML_NODE_FONT << XML_NODE_SERVER << XML_NODE_COLOR);


    mHistory = new ChatHistory(this, mChatType);
    mHistory->setAlignment(Qt::AlignLeft | Qt::AlignBottom);
    mHistory->setReadOnly(true);


    mUserlist = new ChatUserlist(this, mChatType);
    mUserlist->setAlignment(Qt::AlignLeft | Qt::AlignBottom);
    mUserlist->setReadOnly(true);


    QObject::connect(Client::instance(), SIGNAL(chatMessage(const QString&)),    this, SLOT(onChatMessage(const QString&)));
    QObject::connect(Client::instance(), SIGNAL(chatUserOnline(const QString&)), this, SLOT(onChatUserOnline(const QString&)));
    QObject::connect(Client::instance(), SIGNAL(chatUserJoined(const QString&)), this, SLOT(onChatUserJoined(const QString&)));
    QObject::connect(Client::instance(), SIGNAL(chatUserLeft(const QString&)),   this, SLOT(onChatUserLeft(const QString&)));
}

void Chat::updatePos(OrientationStatus orientation)
{
    //qDebug() << "Chat::updatePos: " << orientation;

    QString chatNode = (mChatType == CT_COMMON_CHAT) ? XML_NODE_COMMON_CHAT : XML_NODE_TABLE_CHAT;

    QString orientNode = XML_NODE_LANDSCAPE; //(orientation == OrientationVertical) ? XML_NODE_PORTRAIT : XML_NODE_LANDSCAPE;

    int x      = XML::instance().readValue(XML_ITEMS_FILENAME, QList<QString>() << chatNode << orientNode << XML_NODE_X).toInt();
    int y      = XML::instance().readValue(XML_ITEMS_FILENAME, QList<QString>() << chatNode << orientNode << XML_NODE_Y).toInt();
    int width  = XML::instance().readValue(XML_ITEMS_FILENAME, QList<QString>() << chatNode << orientNode << XML_NODE_WIDTH).toInt();
    int height = XML::instance().readValue(XML_ITEMS_FILENAME, QList<QString>() << chatNode << orientNode << XML_NODE_HEIGHT).toInt();

    move(x, y);
    setFixedSize(width, height);

    // HISTORY
    int xHistory      = XML::instance().readValue(XML_ITEMS_FILENAME, QList<QString>() << chatNode << XML_NODE_HISTORY << orientNode << XML_NODE_X).toInt();
    int yHistory      = XML::instance().readValue(XML_ITEMS_FILENAME, QList<QString>() << chatNode << XML_NODE_HISTORY << orientNode << XML_NODE_Y).toInt();
    int widthHistory  = XML::instance().readValue(XML_ITEMS_FILENAME, QList<QString>() << chatNode << XML_NODE_HISTORY << orientNode << XML_NODE_WIDTH).toInt();
    int heightHistory = XML::instance().readValue(XML_ITEMS_FILENAME, QList<QString>() << chatNode << XML_NODE_HISTORY << orientNode << XML_NODE_HEIGHT).toInt();

    mHistory->move(xHistory, yHistory);
    mHistory->setMinimumSize(widthHistory, heightHistory);

    // USERLIST
    int xUserlist      = XML::instance().readValue(XML_ITEMS_FILENAME, QList<QString>() << chatNode << XML_NODE_USERLIST << orientNode << XML_NODE_X).toInt();
    int yUserlist      = XML::instance().readValue(XML_ITEMS_FILENAME, QList<QString>() << chatNode << XML_NODE_USERLIST << orientNode << XML_NODE_Y).toInt();
    int widthUserlist  = XML::instance().readValue(XML_ITEMS_FILENAME, QList<QString>() << chatNode << XML_NODE_USERLIST << orientNode << XML_NODE_WIDTH).toInt();
    int heightUserlist = XML::instance().readValue(XML_ITEMS_FILENAME, QList<QString>() << chatNode << XML_NODE_USERLIST << orientNode << XML_NODE_HEIGHT).toInt();

    mUserlist->move(xUserlist, yUserlist);
    mUserlist->setMinimumSize(widthUserlist, heightUserlist);

}

void Chat::show()
{
    TABLEID tableID = (mChatType == CT_COMMON_CHAT) ?
                      COMMON_CHAT_ID : UI::instance()->getGameTable();

    Client::instance()->joinChat(LOGIC_ID_CHESS, tableID);
    QDialog::show();
}

bool Chat::close()
{
    TABLEID tableID = (mChatType == CT_COMMON_CHAT) ?
                      COMMON_CHAT_ID : UI::instance()->getGameTable();

    Client::instance()->leaveChat(LOGIC_ID_CHESS, tableID);
    return QDialog::close();
}

void Chat::ChatHistory::mouseReleaseEvent(QMouseEvent * event)
{
    MainWindow::instance()->showChatMessageDialog(mChatType);
}

void Chat::ChatUserlist::mouseReleaseEvent(QMouseEvent * event)
{

}


void Chat::onChatMessage(const QString& message)
{
    QString htmlText = mHistory->toHtml();

    QString username = Client::instance()->username() + ":";
    if(message.left(username.length()) == username)
    {
         htmlText += "<font color=\"" + mColorMe + "\">" + message + "</font>";
    }
    else
    {
        htmlText += "<font color=\""+ mColorOpponent + "\">" + message + "</font>";
    }

    mHistory->setHtml(htmlText);
    mHistory->adjustSize();
    mHistory->verticalScrollBar()->setSliderPosition(mHistory->verticalScrollBar()->maximum());
}

void Chat::onChatUserOnline(const QString& userName)
{
    onChatUserJoined(userName);
}

void Chat::onChatUserJoined(const QString& userName)
{
    if(!mUserlist->mNames.contains(userName))
    {
        mUserlist->mNames.append(userName);
    }


    QString htmlText = "<font color=\""+ mColorServer + "\">";
    for(int i=0; i<mUserlist->mNames.size(); i++)
    {
        htmlText += mUserlist->mNames[i] + "\n";
    }
    htmlText += "</font>";

    mUserlist->setHtml(htmlText);
    mUserlist->adjustSize();
    mUserlist->verticalScrollBar()->setSliderPosition(mUserlist->verticalScrollBar()->minimum());
}

void Chat::onChatUserLeft  (const QString& userName)
{
    if(!mUserlist->mNames.contains(userName))
    {
        mUserlist->mNames.removeAll(userName);
    }

    QString htmlText = "<font color=\""+ mColorServer + "\">";
    for(int i=0; i<mUserlist->mNames.size(); i++)
    {
        htmlText += mUserlist->mNames[i] + "\n";
    }
    htmlText += "</font>";

    mUserlist->setHtml(htmlText);
    mUserlist->adjustSize();
    mUserlist->verticalScrollBar()->setSliderPosition(mUserlist->verticalScrollBar()->minimum());
}



Chat::~Chat()
{
}
