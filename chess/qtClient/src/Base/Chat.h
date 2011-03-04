#ifndef CHAT_H_
#define CHAT_H_

#include <QLineEdit>
#include <QScrollArea>
#include <QLabel>
#include <QTextEdit>
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

    // derived from QWidged
    void show();
    bool close();

private:

    class ChatHistory: public QTextEdit
    {
    public:
        ChatHistory(QWidget* parent, ChatType type): QTextEdit(parent), mChatType(type)  { }
    protected:
        virtual void mouseReleaseEvent(QMouseEvent * event);
    private:
        ChatType mChatType;
    };

    QGraphicsTextItem* mHeader;
    QGraphicsScene* mParentScene;

    ChatHistory* mHistory;
//    QScrollArea* mScrollArea;
    QGraphicsRectItem* mBorder;

    ChatType mChatType;

    // colors
    QString mColorMe;
    QString mColorOpponent;
    QString mColorServer;

private slots:

    void onChatMessage(const QString& message);
    void onChatNote(const QString& message);

};

#endif /* CHAT_H_ */
