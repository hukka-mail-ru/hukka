/*
 * MainMenu.cpp
 *
 *  Created on: Apr 7, 2010
 *      Author: ssy
 */

#include <QGraphicsPixmapItem>
#include <QGraphicsItem>
#include <QDebug>
#include <QObject>
#include <QList>
#include <QDir>
#include <assert.h>

#include <XML.h>
#include "MainMenu.h"
#include "MainWindow.h"
#include "Exception.h"
#include "Button.h"
#include "Client.h"
#include "UI.h"
#include "Defines.h"
#include "Pixmaps.h"





MainMenu::MainMenu(QObject *parent):
    QGraphicsScene(parent),
    mChat(NULL)
{
    mSplash = addPixmap(Pixmaps::get(PIX_SPLASH));
    mSplash->setZValue(Z_CELLS_LAYER);


    // scene
    int scene_x      = XML::instance().readValue(XML_ITEMS_FILENAME, QList<QString>() << XML_NODE_SCENE << XML_NODE_X).toInt();
    int scene_y      = XML::instance().readValue(XML_ITEMS_FILENAME, QList<QString>() << XML_NODE_SCENE << XML_NODE_Y).toInt();
    int scene_width  = XML::instance().readValue(XML_ITEMS_FILENAME, QList<QString>() << XML_NODE_SCENE << XML_NODE_WIDTH).toInt();
    int scene_height = XML::instance().readValue(XML_ITEMS_FILENAME, QList<QString>() << XML_NODE_SCENE << XML_NODE_HEIGHT).toInt();

   // qDebug() << scene_x << scene_y << scene_width << scene_height;
    setSceneRect( scene_x, scene_y, scene_width, scene_height );

    // buttons
    createGameButton = newButton(Pixmaps::get(PIX_BUTTON_CREATE_GAME), SLOT(onCreateGameClicked()), tr("New Game"), XML_NODE_NEW_GAME);
    findGameButton   = newButton(Pixmaps::get(PIX_BUTTON_FIND_GAME),   SLOT(onFindGameClicked()), tr("Find game"), XML_NODE_FIND_GAME);
    optionsButton    = newButton(Pixmaps::get(PIX_BUTTON_OPTIONS),     SLOT(onOptionsClicked()), tr("Options"), XML_NODE_OPTIONS);
    walletButton     = newButton(Pixmaps::get(PIX_BUTTON_WALLET),      SLOT(onWalletClicked()), tr("Wallet"), XML_NODE_WALLET);
    exitButton       = newButton(Pixmaps::get(PIX_BUTTON_EXIT),        SLOT(onExitClicked()), tr(""), XML_NODE_EXIT);

    // splash
    int x = XML::instance().readValue(XML_ITEMS_FILENAME, QList<QString>() << XML_NODE_SPLASH << XML_NODE_X).toInt();
    int y = XML::instance().readValue(XML_ITEMS_FILENAME, QList<QString>() << XML_NODE_SPLASH << XML_NODE_Y).toInt();

    mSplash->setPos(x, y);

    // text items
    mPlayerNameText = newTextItem(XML_NODE_PLAYER);
    mPlayerRatingText = newTextItem(XML_NODE_RATING);
    mPlayerBalanceText = newTextItem(XML_NODE_BALANCE);
}


QGraphicsTextItem* MainMenu::newTextItem(const QString& xmlNode)
{
    QString family = XML::instance().readValue(XML_ITEMS_FILENAME, QList<QString>() << XML_NODE_MAIN_MENU << xmlNode << XML_NODE_FONT << XML_NODE_FAMILY);
    int size  =      XML::instance().readValue(XML_ITEMS_FILENAME, QList<QString>() << XML_NODE_MAIN_MENU << xmlNode << XML_NODE_FONT << XML_NODE_SIZE).toInt();
    QString color =  XML::instance().readValue(XML_ITEMS_FILENAME, QList<QString>() << XML_NODE_MAIN_MENU << xmlNode << XML_NODE_FONT << XML_NODE_COLOR);
    int x =          XML::instance().readValue(XML_ITEMS_FILENAME, QList<QString>() << XML_NODE_MAIN_MENU << xmlNode << XML_NODE_X).toInt();
    int y =          XML::instance().readValue(XML_ITEMS_FILENAME, QList<QString>() << XML_NODE_MAIN_MENU << xmlNode << XML_NODE_Y).toInt();

    QGraphicsTextItem* item = addText("",QFont(family, size));
    item->setPos(x, y);
    item->setDefaultTextColor( QColor(color) );
    item->setZValue(Z_TEXT_LAYER);

    return item;
}

Button* MainMenu::newButton(const QPixmap& pixmap, const char* slot,
                            const QString& text, const QString& xmlNodeName)
{
    Button* button = new Button(this, pixmap, text, XML_NODE_BUTTONS, xmlNodeName);
    QObject::connect(button, SIGNAL(clicked()), this, slot);
    return button;
}


void MainMenu::enableItems()
{
    QList<QGraphicsItem*> list = items();

    for(int i=0; i<list.size(); i++)
    {
        list[i]->setOpacity(OPAQUE_NORMAL);
    }
}

void MainMenu::disableItems()
{
    QList<QGraphicsItem*> list = items();

    for(int i=0; i<list.size(); i++)
    {
        list[i]->setOpacity(OPAQUE_HALF);
    }
}


void MainMenu::onCreateGameClicked()
{
//    MainWindow::instance()->showPromotionDialog(PC_WHITE);
//    MainWindow::instance()->showPromotionDialog(PC_BLACK);

    MainWindow::instance()->setMode(MW_WAIT);

    mClickedButton = createGameButton;

    connectToGameServer();
}

void MainMenu::onFindGameClicked()
{
    MainWindow::instance()->setMode(MW_WAIT);

    mClickedButton = findGameButton;

    connectToGameServer();
}

void MainMenu::onWalletClicked()
{
    MainWindow::instance()->setMode(MW_WAIT);

    mClickedButton = walletButton;

    connectToGameServer();
}

/*
void MainMenu::onChatClicked()
{
    if(!mChat)
    {
        MainWindow::instance()->setMode(MW_WAIT);
        mClickedButton = chatButton;
        connectToGameServer();
    }
    else if(mChat->getState() == CHAT_CLOSED)
    {
        mChat->show();
    }
    else
    {
        // toggle chat visibility
        mChat->setVisible( !mChat->isVisible() );
    }
}
*/

void MainMenu::close()
{
   if(mChat)
       mChat->close();
}


void MainMenu::connectToGameServer()
{
    QString serverName = XML::instance().readValue(XML_CONFIG_FILENAME, QList<QString>() << XML_NODE_SERVER << XML_NODE_NAME);
    QString serverPort = XML::instance().readValue(XML_CONFIG_FILENAME, QList<QString>() << XML_NODE_SERVER << XML_NODE_PORT);

    connect(Client::instance(), SIGNAL(connectedToHost()), this, SLOT(onConnectedToHost()));
    Client::instance()->connectToHost(DEFAULT_PROXY, serverName, serverPort.toInt());
}

void MainMenu::onConnectedToHost()
{
    disconnect(Client::instance(), SIGNAL(connectedToHost()), this, SLOT(onConnectedToHost()));
    assert(mClickedButton);

    if(UI::instance()->isPlayerAuthorized())
    {
        onAuthorized();
    }
    else
    {
        QString login = XML::instance().readValue(XML_CONFIG_FILENAME, QList<QString>() << XML_NODE_USER << XML_NODE_LOGIN);
        QString pwd = XML::instance().readValue(XML_CONFIG_FILENAME, QList<QString>() << XML_NODE_USER << XML_NODE_PASSWORD);

        UI::instance()->setPlayerName(PT_ME, login);

        connect(Client::instance(), SIGNAL(authorized()), this, SLOT(onAuthorized()));
        connect(Client::instance(), SIGNAL(notAuthorized(const QString&)), this, SLOT(onNotAuthorized(const QString&)));

        Client::instance()->login(login, pwd);

    }
}

void MainMenu::onNotAuthorized(const QString& what)
{
    MainWindow::instance()->showError(what);
    MainWindow::instance()->showAuthorizationDialog();
}


void MainMenu::onAuthorized()
{
    disconnect(Client::instance(), SIGNAL(registered()), this, SLOT(onAuthorized()));
    disconnect(Client::instance(), SIGNAL(authorized()), this, SLOT(onAuthorized()));
    disconnect(Client::instance(), SIGNAL(notAuthorized(const QString&)), this, SLOT(onNotAuthorized(const QString&)));

  //  qDebug() << "MainMenu::onAuthorized";

    assert(mClickedButton);
    UI::instance()->setPlayerAuthorized(true);

    if(mClickedButton == createGameButton ||
       mClickedButton == findGameButton )
    {
        MainWindow::instance()->setMode(MW_WAIT);
        // for case if client recreates the game table after a crash
        connect(Client::instance(), SIGNAL(gotMyGameTable(TABLEID, bool)), this, SLOT(onGotMyGameTable(TABLEID, bool)));
        Client::instance()->getMyGameTable(LOGIC_ID_CHESS);
    }

    /*
    else if(mClickedButton == chatButton && !mChat)
    {
        mChat = new Chat(MainWindow::instance(), CT_COMMON_CHAT);
        mChat->show();

        MainWindow::instance()->setMode(MW_NORMAL);
    }*/

    connect(Client::instance(), SIGNAL(gotMyRating(unsigned)), this, SLOT(onGotMyRating(unsigned)));
    Client::instance()->getMyRating();

    connect(Client::instance(), SIGNAL(gotMyBalance(unsigned)), this, SLOT(onGotMyBalance(unsigned)));
    Client::instance()->getMyBalance();

}

void MainMenu::onGotMyRating(unsigned myRating)
{
    UI::instance()->setPlayerRating(PT_ME, myRating);

    mPlayerNameText->setPlainText(UI::instance()->getPlayer(PT_ME).name);

    QString ratingText =  (myRating == RATING_NOT_AVAILABLE) ?
                          tr("Rating: not available") :
                          tr("Rating: ") + QString::number(myRating);

    mPlayerRatingText->setPlainText(ratingText);
}

void MainMenu::onGotMyBalance(unsigned myBalance)
{
    UI::instance()->setPlayerBalance(PT_ME, myBalance);

    mPlayerBalanceText->setPlainText(tr("Balance: ") + QString::number(myBalance));

    if(mClickedButton == walletButton)
    {
        MainWindow::instance()->showWalletDialog();
        mClickedButton = NULL;
    }
}

void MainMenu::onGotMyGameTable(TABLEID tableID, bool isOwner)
{
    disconnect(Client::instance(), SIGNAL(gotMyGameTable(TABLEID, bool)), this, SLOT(onGotMyGameTable(TABLEID, bool)));

    qDebug() << "isOwner: " << isOwner;

    if(tableID)
    {
        UI::instance()->setGameTable(tableID);

        connect(Client::instance(), SIGNAL(gotOpponent(const Player&)), this, SLOT(onGotOpponent(const Player&)));
        Client::instance()->getOpponent(tableID);
    }
    else
    {
        connect(Client::instance(), SIGNAL(gotLastGameResult(unsigned)), this, SLOT(onGotLastGameResult(unsigned)));
        Client::instance()->getLastGameResult();

    }


}


void MainMenu::onGotLastGameResult(unsigned result)
{
    disconnect(Client::instance(), SIGNAL(gotLastGameResult(unsigned)), this, SLOT(onGotLastGameResult(unsigned)));

    QString finished = tr("The last game has been finished.") + "\n\n" +
                       Global::getGameResultText(result, UI::instance()->getPlayer(PT_ME).rating);


    if(result && result != P_NONE)
    {
        MainWindow::instance()->showMessage(finished);

        Client::instance()->deleteLastGameResult();
    }

    if(mClickedButton == createGameButton)
    {
        MainWindow::instance()->showCreateGameDialog();
    }
    else if(mClickedButton == findGameButton)
    {
        MainWindow::instance()->showFindGameDialog();
    }
}


void MainMenu::onGotOpponent(const Player& opponent)
{
   disconnect(Client::instance(), SIGNAL(gotOpponent(const Player&)), this, SLOT(onGotOpponent(const Player&)));
   UI::instance()->setPlayerName(PT_OPPONENT, opponent.name);
   UI::instance()->setPlayerRating(PT_OPPONENT, opponent.rating);

   MainWindow::instance()->showMessage(
           tr("You have an unfinished game. Please finish it."));

   Client::instance()->setGameStatus(GAM_STARTED);

   MainWindow::instance()->setMode(MW_WAIT);

   UI::instance()->setPlayerColor(PT_ME, PC_WHITE);
  // MainWindow::instance()->showGameScene(PC_WHITE);

   UI::instance()->startGame();

}



void MainMenu::onOptionsClicked()
{
    MainWindow::instance()->showOptionsDialog();

  //  connect(Client::instance(), SIGNAL(disconnectedFromHost()), this, SLOT(onDisonnectedFromHost()));
    // should be the last operation in this method
  //  Client::instance()->disconnectFromHost();
}


void MainMenu::onDisonnectedFromHost()
{
   // qDebug() << "MainWindow::onDisonnectedFromHost()";
    UI::instance()->shutdown();
}

void MainMenu::onExitClicked()
{
    close();
    UI::instance()->shutdown();
}


