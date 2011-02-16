#ifndef CHAT_H_
#define CHAT_H_

#include <QLineEdit>
#include <QScrollArea>
#include <QLabel>
#include <QPushButton>
#include <QGraphicsScene>
#include <MyDialog.h>

#include <QGraphicsPixmapItem>
#include <Defines.h>
#include <orientation.h>


class Chat: public QObject
{
Q_OBJECT
public:
    Chat(QGraphicsScene* parentScene, ChatType type);

    void updatePos(OrientationStatus orientation);

private:

    class ChatHistory: public QLabel
    {
    public:
        ChatHistory(ChatType type): mChatType(type) {}
    protected:
        virtual void mouseReleaseEvent(QMouseEvent * event);
    private:
        ChatType mChatType;
    };

    QGraphicsTextItem* mHeader;
    QGraphicsScene* mParentScene;

    ChatHistory* mHistory;
    QScrollArea* mScrollArea;
    QGraphicsRectItem* mBorder;

    ChatType mChatType;

private slots:

    void onChatMessage(const QString& message);

};

#endif /* CHAT_H_ */
