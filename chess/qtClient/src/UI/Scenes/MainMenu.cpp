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
    else
    {
        if(mChat->isVisible())
            mChat->hide();
        else
            mChat->show();
    }
}

void MainMenu::hide()
{
   if(mChat && mChat->isVisible())
       mChat->hide();
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
        connect(Client::instance(), SIGNAL(gotMyGameTable(TABLEID)), this, SLOT(onGotMyGameTable(TABLEID)));
        Client::instance()->getMyGameTable(LOGIC_ID_CHESS);
    }
    else if(mClickedButton == chatButton && !mChat)
    {
        Client::instance()->joinCommonChat(LOGIC_ID_CHESS);

        mChat = new Chat(MainWindow::instance(), CT_COMMON_CHAT);
        mChat->updatePos(MainWindow::instance()->getOrientation());
        mChat->show();

        MainWindow::instance()->setMode(MW_NORMAL);
    }
}

void MainMenu::onGotMyGameTable(TABLEID id)
{
    disconnect(Client::instance(), SIGNAL(gotMyGameTable(TABLEID)), this, SLOT(onGotMyGameTable(TABLEID)));

    if(id)
    {
        MainWindow::instance()->showMessage(
                tr("You have an unfinished game. Please finish it."));

        Client::instance()->setGameStatus(GAM_STARTED);

        UI::instance()->setGameTable(id);

        MainWindow::instance()->showGameScene(PC_WHITE);

        UI::instance()->startGame();
    }
    else
    {
        if(mClickedButton == createGameButton)
        {
            MainWindow::instance()->showCreateGameDialog();
        }
        else if(mClickedButton == findGameButton)
        {
            MainWindow::instance()->showFindGameDialog();
        }
    }
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
    UI::instance()->shutdown();
}


