
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
    ~MainMenu();

    void initialize();

    void enableItems();
    void disableItems();

    void updateItemsPositions(OrientationStatus orientation);

private:

    void loadImages();
    void connectToGameServer();

    Button* createGameButton;
    Button* findGameButton;
    Button* chatButton;
    Button* optionsButton;
    Button* exitButton;

    Button* mClickedButton;

    QGraphicsPixmapItem* mSplash;

    Chat* mChat;

    bool mHostGame;

private slots:
    Button* newButton(const QPixmap& pixmap, const char* slot,
                      const QString& text, const QString& xmlNodeName);

    void onConnectedToHost();
    void onDisonnectedFromHost();
    void onAuthorized();

    void onCreateGameClicked();
    void onFindGameClicked();
    void onChatClicked();
    void onOptionsClicked();
    void onExitClicked();


};

#endif /* MAINMENU_H_ */
