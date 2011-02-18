#include <Chat.h>
#include <QScrollArea>
#include <QScrollBar>
#include <QVBoxLayout>
#include <QHBoxLayout>
#include <Client.h>
#include <UI.h>
#include <MainWindow.h>
#include <XML.h>


Chat::Chat(QWidget* parent, ChatType type):
    QDialog(parent), mChatType(type)
{
    // No title, no [X] button, no bottom
    setWindowFlags(Qt::Widget);

    setStyleSheet("Chat        { background: black; } "
                  "QScrollArea { background: black; font-size: 18px; } "
                  "QLabel      { background: black; font-size: 18px; color: white; }"
                  );

    // HEADER
    QString chatNode = (mChatType == CT_COMMON_CHAT) ? XML_NODE_COMMON_CHAT : XML_NODE_TABLE_CHAT;
    QString family = XML::instance().readValue(XML_ITEMS_FILENAME, QList<QString>() << chatNode << XML_NODE_FONT << XML_NODE_FAMILY);
    int size =       XML::instance().readValue(XML_ITEMS_FILENAME, QList<QString>() << chatNode << XML_NODE_FONT << XML_NODE_SIZE).toInt();
    QString color  = XML::instance().readValue(XML_ITEMS_FILENAME, QList<QString>() << chatNode << XML_NODE_FONT << XML_NODE_COLOR);

    mHistory = new ChatHistory(this, mChatType);
    mHistory->setAlignment(Qt::AlignLeft | Qt::AlignBottom);
    mHistory->setWordWrap(true);

    mScrollArea = new QScrollArea(this);
    mScrollArea->setAlignment(Qt::AlignLeft | Qt::AlignBottom);
    mScrollArea->setWidget(mHistory);

    if(mChatType == CT_TABLE_CHAT)
    {
        QObject::connect(Client::instance(), SIGNAL(tableChatMessage(const QString&)), this, SLOT(onChatMessage(const QString&)));
    }
    else if(mChatType == CT_COMMON_CHAT)
    {
        QObject::connect(Client::instance(), SIGNAL(commonChatMessage(const QString&)), this, SLOT(onChatMessage(const QString&)));
    }



}

void Chat::updatePos(OrientationStatus orientation)
{
    qDebug() << "Chat::updatePos: " << orientation;

    QString chatNode = (mChatType == CT_COMMON_CHAT) ? XML_NODE_COMMON_CHAT : XML_NODE_TABLE_CHAT;

    QString orientNode = XML_NODE_LANDSCAPE; //(orientation == OrientationVertical) ? XML_NODE_PORTRAIT : XML_NODE_LANDSCAPE;

    QList<QString> path;
    int x      = XML::instance().readValue(XML_ITEMS_FILENAME, path << chatNode << orientNode << XML_NODE_X).toInt(); path.clear();
    int y      = XML::instance().readValue(XML_ITEMS_FILENAME, path << chatNode << orientNode << XML_NODE_Y).toInt(); path.clear();
    int width  = XML::instance().readValue(XML_ITEMS_FILENAME, path << chatNode << orientNode << XML_NODE_WIDTH).toInt(); path.clear();
    int height = XML::instance().readValue(XML_ITEMS_FILENAME, path << chatNode << orientNode << XML_NODE_HEIGHT).toInt(); path.clear();

    int textOffset   = XML::instance().readValue(XML_ITEMS_FILENAME, path << chatNode << XML_NODE_TEXT_OFFSET).toInt(); path.clear();
    int borderWidth  = XML::instance().readValue(XML_ITEMS_FILENAME, path << chatNode << XML_NODE_BORDER << XML_NODE_WIDTH).toInt(); path.clear();

    move(x, y);
    setFixedSize(width, height);

    mHistory->setMinimumSize(width, height);
    mScrollArea->setFixedSize(width, height);
}



void Chat::ChatHistory::mouseReleaseEvent(QMouseEvent * event)
{
    MainWindow::instance()->showChatMessageDialog(mChatType);
}


void Chat::onChatMessage(const QString& originalMessage)
{
    QString message = originalMessage;
    // 'text' -> text
    if(message.startsWith('\'') && message.endsWith('\''))
    {
        int len = message.length() - 2; // 2  ' symbols
        message = message.mid(1, len);
    }

    QString text = mHistory->text() + "\n" + message;
    mHistory->setText(text);
    mHistory->adjustSize();

    QScrollBar* vsBar = mScrollArea->verticalScrollBar();
    vsBar->setSliderPosition(vsBar->maximum());
}


Chat::~Chat()
{
}
