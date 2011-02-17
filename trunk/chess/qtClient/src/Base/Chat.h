#ifndef CHAT_H_
#define CHAT_H_

#include <QLineEdit>
#include <QScrollArea>
#include <QLabel>
#include <QPushButton>
#include <QDialog>
#include <QGraphicsPixmapItem>
#include <Defines.h>
#include <orientation.h>


class Chat: public QDialog
{
Q_OBJECT
public:
    Chat(QWidget* parent, ChatType type);

    ~Chat();

    void updatePos(OrientationStatus orientation);

    void close();

private:

    class ChatHistory: public QLabel
    {
    public:
        ChatHistory(QWidget* parent, ChatType type): QLabel(parent), mChatType(type)  { }
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
