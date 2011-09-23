
#ifndef MAINMENU_H_
#define MAINMENU_H_

#include <QGraphicsScene>
#include <QGraphicsItem>
#include <QString>
#include <Button.h>
#include <Chat.h>
#include <orientation.h>


class MainMenu: public QGraphicsScene
{
Q_OBJECT
public:
    MainMenu(QObject *parent = 0);
    ~MainMenu() {}

    void enableItems();
    void disableItems();

    void close();

private:

    void connectToGameServer();

    Button* newButton(const QPixmap& pixmap, const char* slot,
                      const QString& text, const QString& xmlNodeName);

    Button* createGameButton;
    Button* findGameButton;
    Button* chatButton;
    Button* optionsButton;
    Button* exitButton;

    Button* mClickedButton;

    QGraphicsPixmapItem* mSplash;

    Chat* mChat;

    QGraphicsTextItem* mPlayerNameText;
    QGraphicsTextItem* mPlayerRatingText;

private slots:

    void onConnectedToHost();
    void onDisonnectedFromHost();
    void onAuthorized();
    void onNotAuthorized(const QString& what);

    void onCreateGameClicked();
    void onFindGameClicked();
    void onChatClicked();
    void onOptionsClicked();
    void onExitClicked();
    void onGotMyGameTable(TABLEID id, bool isOwner);
    void onGotMyRating(quint32 myRating);
    void onGotOpponent(const Player& opponent);
    void onGotLastGameResult(int result);


};

#endif /* MAINMENU_H_ */
