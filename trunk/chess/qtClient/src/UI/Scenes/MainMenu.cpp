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
}


MainMenu::~MainMenu()
{
  // qDebug() << "MainMenu::~MainMenu()";
}


Button* MainMenu::newButton(const QPixmap& pixmap, const char* slot,
                            const QString& text, const QString& xmlNodeName)
{
    Button* button = new Button(this, pixmap, text, xmlNodeName);
    QObject::connect(button, SIGNAL(clicked()), this, slot);
    return button;
}



void MainMenu::initialize()
{
    mSplash = addPixmap(Pixmaps::get(PIX_SPLASH));
    mSplash->setZValue(Z_CELLS_LAYER);


    createGameButton = newButton(Pixmaps::get(PIX_BUTTON_CREATE_GAME), SLOT(onCreateGameClicked()), tr("New Game"), XML_NODE_NEW_GAME);
    findGameButton   = newButton(Pixmaps::get(PIX_BUTTON_FIND_GAME),   SLOT(onFindGameClicked()), tr("Find game"), XML_NODE_FIND_GAME);
    chatButton       = newButton(Pixmaps::get(PIX_BUTTON_CHAT),        SLOT(onChatClicked()), tr("Chat"), XML_NODE_CHAT);
    optionsButton    = newButton(Pixmaps::get(PIX_BUTTON_OPTIONS),     SLOT(onOptionsClicked()), tr("Options"), XML_NODE_OPTIONS);
    exitButton       = newButton(Pixmaps::get(PIX_BUTTON_EXIT),        SLOT(onExitClicked()), "", XML_NODE_EXIT);

}


void MainMenu::updateItemsPositions(OrientationStatus orientation)
{
    // TODO copy-paste!
    QString orientNode = (orientation == OrientationHorizontal) ? XML_NODE_LANDSCAPE : XML_NODE_PORTRAIT;


    // scene
    int scene_x      = XML::instance().readValue(XML_ITEMS_FILENAME, QList<QString>() << XML_NODE_SCENE << orientNode << XML_NODE_X).toInt();
    int scene_y      = XML::instance().readValue(XML_ITEMS_FILENAME, QList<QString>() << XML_NODE_SCENE << orientNode << XML_NODE_Y).toInt();
    int scene_width  = XML::instance().readValue(XML_ITEMS_FILENAME, QList<QString>() << XML_NODE_SCENE << orientNode << XML_NODE_WIDTH).toInt();
    int scene_height = XML::instance().readValue(XML_ITEMS_FILENAME, QList<QString>() << XML_NODE_SCENE << orientNode << XML_NODE_HEIGHT).toInt();

   // qDebug() << scene_x << scene_y << scene_width << scene_height;
    setSceneRect( scene_x, scene_y, scene_width, scene_height );

    // buttons
    createGameButton->updatePos(orientation);
    findGameButton->updatePos(orientation);
    chatButton->updatePos(orientation);
    optionsButton->updatePos(orientation);
    exitButton->updatePos(orientation);

    // splash
    int x = XML::instance().readValue(XML_ITEMS_FILENAME, QList<QString>() << XML_NODE_SPLASH << orientNode << XML_NODE_X).toInt();
    int y = XML::instance().readValue(XML_ITEMS_FILENAME, QList<QString>() << XML_NODE_SPLASH << orientNode << XML_NODE_Y).toInt();

    mSplash->setPos(x, y);

    // text (player name)
    QString player_family = XML::instance().readValue(XML_ITEMS_FILENAME, QList<QString>() << XML_NODE_MAIN_MENU << XML_NODE_PLAYER << XML_NODE_FONT << XML_NODE_FAMILY);
    int player_size  =      XML::instance().readValue(XML_ITEMS_FILENAME, QList<QString>() << XML_NODE_MAIN_MENU << XML_NODE_PLAYER << XML_NODE_FONT << XML_NODE_SIZE).toInt();
    QString player_color =  XML::instance().readValue(XML_ITEMS_FILENAME, QList<QString>() << XML_NODE_MAIN_MENU << XML_NODE_PLAYER << XML_NODE_FONT << XML_NODE_COLOR);
    int player_x =          XML::instance().readValue(XML_ITEMS_FILENAME, QList<QString>() << XML_NODE_MAIN_MENU << XML_NODE_PLAYER << orientNode << XML_NODE_X).toInt();
    int player_y =          XML::instance().readValue(XML_ITEMS_FILENAME, QList<QString>() << XML_NODE_MAIN_MENU << XML_NODE_PLAYER << orientNode << XML_NODE_Y).toInt();

    mPlayerNameText = addText("",QFont(player_family, player_size));
    mPlayerNameText->setPos(player_x, player_y);
    mPlayerNameText->setDefaultTextColor( QColor(player_color) );
    mPlayerNameText->setZValue(Z_TEXT_LAYER);

    // text (rating)
    QString rating_family = XML::instance().readValue(XML_ITEMS_FILENAME, QList<QString>() << XML_NODE_MAIN_MENU << XML_NODE_RATING << XML_NODE_FONT << XML_NODE_FAMILY);
    int rating_size =       XML::instance().readValue(XML_ITEMS_FILENAME, QList<QString>() << XML_NODE_MAIN_MENU << XML_NODE_RATING << XML_NODE_FONT << XML_NODE_SIZE).toInt();
    QString rating_color =  XML::instance().readValue(XML_ITEMS_FILENAME, QList<QString>() << XML_NODE_MAIN_MENU << XML_NODE_RATING << XML_NODE_FONT << XML_NODE_COLOR);
    int rating_x =          XML::instance().readValue(XML_ITEMS_FILENAME, QList<QString>() << XML_NODE_MAIN_MENU << XML_NODE_RATING << orientNode << XML_NODE_X).toInt();
    int rating_y =          XML::instance().readValue(XML_ITEMS_FILENAME, QList<QString>() << XML_NODE_MAIN_MENU << XML_NODE_RATING << orientNode << XML_NODE_Y).toInt();

    mPlayerRatingText = addText("",QFont(rating_family, rating_size));
    mPlayerRatingText->setPos(rating_x, rating_y);
    mPlayerRatingText->setDefaultTextColor( QColor(rating_color) );
    mPlayerRatingText->setZValue(Z_TEXT_LAYER);



  //  qDebug() << "MainMenu::updateItemsPositions " << orientation;
    // chat
    if(mChat)
    {
        mChat->updatePos(orientation);
    }
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
        connect(Client::instance(), SIGNAL(registered()), this, SLOT(onAuthorized()));
        connect(Client::instance(), SIGNAL(authorized()), this, SLOT(onAuthorized()));
        MainWindow::instance()->showAuthorizationDialog();
    }
}

void MainMenu::onAuthorized()
{
    disconnect(Client::instance(), SIGNAL(registered()), this, SLOT(onAuthorized()));
    disconnect(Client::instance(), SIGNAL(authorized()), this, SLOT(onAuthorized()));

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
    else if(mClickedButton == chatButton && !mChat)
    {
        mChat = new Chat(MainWindow::instance(), CT_COMMON_CHAT);
        mChat->updatePos(MainWindow::instance()->getOrientation());
        mChat->show();

        MainWindow::instance()->setMode(MW_NORMAL);
    }

    connect(Client::instance(), SIGNAL(gotMyRating(quint32)), this, SLOT(onGotMyRating(quint32)));
    Client::instance()->getMyRating();
}

void MainMenu::onGotMyRating(quint32 myRating)
{
    UI::instance()->setPlayerRating(PT_ME, myRating);

    mPlayerNameText->setPlainText(UI::instance()->getPlayer(PT_ME).name);

    QString ratingText =  (myRating == RATING_NOT_AVAILABLE) ?
                          "Your rating is not available.\nPlease visit www.site.com\nto learn how to enable it." :
                          "Your rating: " + QString::number(myRating);

    mPlayerRatingText->setPlainText(ratingText);
}

void MainMenu::onGotMyGameTable(TABLEID tableID, bool isOwner)
{
    disconnect(Client::instance(), SIGNAL(gotMyGameTable(TABLEID, bool)), this, SLOT(onGotMyGameTable(TABLEID, bool)));

    qDebug() << "isOwner: " << isOwner;

    if(tableID)
    {
        UI::instance()->setGameTable(tableID);

        connect(Client::instance(), SIGNAL(gotOpponent(const QString&, int)), this, SLOT(onGotOpponent(const QString&, int)));
        Client::instance()->getOpponent(tableID);
    }
    else
    {
        connect(Client::instance(), SIGNAL(gotLastGameResult(int)), this, SLOT(onGotLastGameResult(int)));
        Client::instance()->getLastGameResult();

    }


}


void MainMenu::onGotLastGameResult(int result)
{
    disconnect(Client::instance(), SIGNAL(gotLastGameResult(int)), this, SLOT(onGotLastGameResult(int)));

    QString finished = tr("The last game has been finished. ");
    switch(result)
    {
        case P_WIN:    MainWindow::instance()->showMessage(finished + tr("You have won!")); break;
        case P_LOOSE:  MainWindow::instance()->showMessage(finished + tr("You have lost.")); break;
        case P_DRAW:   MainWindow::instance()->showMessage(finished + tr("A draw.")); break;
        default: break;
    }

    if(result && result != P_NO_RES)
    {
        Client::instance()->deleteLastGameResult();
    }

    if(mClickedButton == createGameButton)
    {
//            UI::instance()->setOwner(true);
        MainWindow::instance()->showCreateGameDialog();
    }
    else if(mClickedButton == findGameButton)
    {
//            UI::instance()->setOwner(false);
        MainWindow::instance()->showFindGameDialog();
    }
}


void MainMenu::onGotOpponent(const QString& opponentName, int opponentRating)
{
    disconnect(Client::instance(), SIGNAL(gotOpponent(const QString&, int)), this, SLOT(onGotOpponent(const QString&, int)));
   UI::instance()->setPlayerName(PT_OPPONENT, opponentName);
   UI::instance()->setPlayerRating(PT_OPPONENT, opponentRating);

   MainWindow::instance()->showMessage(
           tr("You have an unfinished game. Please finish it."));

   Client::instance()->setGameStatus(GAM_STARTED);


   MainWindow::instance()->showGameScene(PC_WHITE);

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


