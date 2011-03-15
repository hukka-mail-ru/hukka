#include <Chat.h>
#include <QScrollArea>
#include <QScrollBar>
#include <QVBoxLayout>
#include <QHBoxLayout>
#include <QTableWidgetItem>
#include <QHeaderView>
#include <QWidget>
#include <Client.h>
#include <UI.h>
#include <MainWindow.h>
#include <XML.h>
#include <chatserver/chatdefs.h>

Chat::~Chat()
{
}

Chat::Chat(QWidget* parent, ChatType type):
    QDialog(parent), mChatType(type), mState(CHAT_CLOSED)
{
    // No title, no [X] button, no bottom
    setWindowFlags(Qt::Widget);

    QString chatNode = (mChatType == CT_COMMON_CHAT) ? XML_NODE_COMMON_CHAT : XML_NODE_TABLE_CHAT;

    QString style =  XML::instance().readValue(XML_ITEMS_FILENAME, QList<QString>() << chatNode << XML_NODE_STYLE);
    setStyleSheet(style);


    mHistory = new History(this, mChatType);
    mUserlist = new Userlist(this, mChatType);

}

void Chat::updatePos(OrientationStatus orientation)
{
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
    mHistory->setFixedSize(widthHistory, heightHistory);

    // USERLIST
    int xUserlist      = XML::instance().readValue(XML_ITEMS_FILENAME, QList<QString>() << chatNode << XML_NODE_USERLIST << orientNode << XML_NODE_X).toInt();
    int yUserlist      = XML::instance().readValue(XML_ITEMS_FILENAME, QList<QString>() << chatNode << XML_NODE_USERLIST << orientNode << XML_NODE_Y).toInt();
    int widthUserlist  = XML::instance().readValue(XML_ITEMS_FILENAME, QList<QString>() << chatNode << XML_NODE_USERLIST << orientNode << XML_NODE_WIDTH).toInt();
    int heightUserlist = XML::instance().readValue(XML_ITEMS_FILENAME, QList<QString>() << chatNode << XML_NODE_USERLIST << orientNode << XML_NODE_HEIGHT).toInt();

    mUserlist->move(xUserlist, yUserlist);
    mUserlist->setFixedSize(widthUserlist, heightUserlist);



}

void Chat::show()
{
    TABLEID tableID = (mChatType == CT_COMMON_CHAT) ?
                      COMMON_CHAT_ID : UI::instance()->getGameTable();

    Client::instance()->joinChat(LOGIC_ID_CHESS, tableID);
    QDialog::show();

    mState = CHAT_OPEN;

    QObject::connect(Client::instance(), SIGNAL(chatMessage(const QString&)),    this, SLOT(onChatMessage(const QString&)));
    QObject::connect(Client::instance(), SIGNAL(chatUserOnline(const QString&)), this, SLOT(onChatUserOnline(const QString&)));
    QObject::connect(Client::instance(), SIGNAL(chatUserJoined(const QString&)), this, SLOT(onChatUserJoined(const QString&)));
    QObject::connect(Client::instance(), SIGNAL(chatUserLeft(const QString&)),   this, SLOT(onChatUserLeft(const QString&)));

}

bool Chat::close()
{
    mState = CHAT_CLOSED;

    mHistory->clear();
    mUserlist->removeAll();

    Client::instance()->leaveChat(LOGIC_ID_CHESS);

    QObject::disconnect(Client::instance(), SIGNAL(chatMessage(const QString&)),    this, SLOT(onChatMessage(const QString&)));
    QObject::disconnect(Client::instance(), SIGNAL(chatUserOnline(const QString&)), this, SLOT(onChatUserOnline(const QString&)));
    QObject::disconnect(Client::instance(), SIGNAL(chatUserJoined(const QString&)), this, SLOT(onChatUserJoined(const QString&)));
    QObject::disconnect(Client::instance(), SIGNAL(chatUserLeft(const QString&)),   this, SLOT(onChatUserLeft(const QString&)));

    return QDialog::close();
}



void Chat::onChatMessage(const QString& message)
{
    QString username = Client::instance()->username() + ":";
    if(message.left(username.length()) == username)
    {
         mHistory->addMessage(message, CS_ME);
    }
    else
    {
         mHistory->addMessage(message, CS_OPPONENT);
    }
}

void Chat::onChatUserOnline(const QString& userName)
{
    mUserlist->addUser(userName);
}

void Chat::onChatUserJoined(const QString& userName)
{
    mHistory->addMessage(userName + " has joined the chat", CS_SERVER);
    mUserlist->addUser(userName);
}

void Chat::onChatUserLeft  (const QString& userName)
{
    mHistory->addMessage(userName + " has left the chat", CS_SERVER);
    mUserlist->removeUser(userName);
}

////////////  History //////////////////////////////////////

Chat::History::History(QWidget* parent, ChatType type):
    QTextEdit(parent), mChatType(type)
{
    setAlignment(Qt::AlignLeft | Qt::AlignBottom);
    setReadOnly(true);

    QString chatNode = (mChatType == CT_COMMON_CHAT) ? XML_NODE_COMMON_CHAT : XML_NODE_TABLE_CHAT;

    mColorMe       = XML::instance().readValue(XML_ITEMS_FILENAME, QList<QString>() << chatNode << XML_NODE_FONT << XML_NODE_ME << XML_NODE_COLOR);
    mColorOpponent = XML::instance().readValue(XML_ITEMS_FILENAME, QList<QString>() << chatNode << XML_NODE_FONT << XML_NODE_OPPONENT << XML_NODE_COLOR);
    mColorServer   = XML::instance().readValue(XML_ITEMS_FILENAME, QList<QString>() << chatNode << XML_NODE_FONT << XML_NODE_SERVER << XML_NODE_COLOR);
}

void Chat::History::mouseReleaseEvent(QMouseEvent * event)
{
    MainWindow::instance()->showChatMessageDialog(mChatType);
}

void Chat::History::addMessage(const QString& message, ChatSender chatSender)
{
    QString color;
    switch(chatSender)
    {
        case CS_ME:        color = mColorMe; break;
        case CS_OPPONENT:  color = mColorOpponent; break;
        case CS_SERVER:    color = mColorServer; break;
    }

    QString htmlText = toHtml() + "<font color=\"" + color + "\">" + message + "</font>";

    setHtml(htmlText);
    adjustSize();
    verticalScrollBar()->setSliderPosition(verticalScrollBar()->maximum());
}


////////////  Userlist //////////////////////////////////////

Chat::Userlist::Userlist(QWidget* parent, ChatType type):
    QTableWidget(0, 1, parent), mChatType(type)
{
    verticalHeader()->hide();
    horizontalHeader()->hide();
    horizontalScrollBar()->hide();
    setEditTriggers(QTableWidget::NoEditTriggers); // read only!
}

void Chat::Userlist::addUser(const QString& userName)
{
    if(!mNames.contains(userName))
    {
        mNames.append(userName);
        updateTable();
    }
}

void Chat::Userlist::removeUser(const QString& userName)
{
    if(mNames.contains(userName))
    {
        mNames.removeOne(userName);
        updateTable();
    }
}

void Chat::Userlist::removeAll()
{
    mNames.clear();
    updateTable();
}

void Chat::Userlist::updateTable()
{
    this->clear();
    this->setRowCount(0);

    for(int i=0; i<mNames.size(); i++)
    {
        insertRow(i);
        setRowHeight(i, 20); // TODO move 20 into XML

        QTableWidgetItem* item = new QTableWidgetItem(mNames[i]);
        setItem(i, 0, item);
    }
}

void Chat::Userlist::mouseReleaseEvent(QMouseEvent * event)
{

}


