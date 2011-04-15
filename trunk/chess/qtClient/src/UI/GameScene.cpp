#include <QDebug>
#include <QGraphicsWidget>
#include <QLabel>
#include <QLineEdit>
#include <QPushButton>
#include "GameScene.h"
#include "Exception.h"
#include "MainWindow.h"
#include <UI.h>
#include <Defines.h>
#include <Pixmaps.h>
#include <XML.h>
#include <Client.h>


GameScene::GameScene(QObject *parent):
       QGraphicsScene(parent),
       mChat(NULL),
       mMeMoveBox(this, PT_ME),
       mOppMoveBox(this, PT_OPPONENT),
       mBoard(this)
{

}

GameScene::~GameScene()
{
    //qDebug() << "GameScene::~GameScene()";
}



void GameScene::showChat()
{
    mChat = new Chat(MainWindow::instance(), CT_TABLE_CHAT);
    mChat->updatePos(OrientationHorizontal);
    //this->addWidget(mChat);
    mChat->show();
}


void GameScene::close()
{
   if(mChat)
       mChat->close();
}

void GameScene::initialize()
{

    // MENU BUTTON
    mMenuButton = new Button(this, Pixmaps::get(PIX_BUTTON_MENU), tr("Game menu"), XML_NODE_BUTTONS, XML_NODE_GAME_MENU);

   // mMenuButton->updatePos(MainWindow::instance()->getOrientation());
    QObject::connect(mMenuButton, SIGNAL(clicked()), this, SLOT(onMenuButtonClicked()));

    mExitButton = new Button(this, Pixmaps::get(PIX_BUTTON_EXIT), "", XML_NODE_BUTTONS, XML_NODE_EXIT);
    QObject::connect(mExitButton, SIGNAL(clicked()), this, SLOT(onExitClicked()));


    connect(Client::instance(), SIGNAL(gotField(Field, bool, bool)), this, SLOT(onGotField(Field, bool, bool)));

}

void GameScene::enableItems()
{
  //  QList<QGraphicsItem*> list = items();

  //  for(int i=0; i<list.size(); i++)
  //  {
  //      list[i]->setOpacity(OPAQUE_NORMAL);
  //  }

    if(mChat)
      mChat->enable();
}

void GameScene::disableItems()
{
 //   QList<QGraphicsItem*> list = items();

 //   for(int i=0; i<list.size(); i++)
 //   {
  //      list[i]->setOpacity(OPAQUE_HALF);
  //  }

    if(mChat)
      mChat->disable();
}

void GameScene::onExitClicked()
{
    close();
    UI::instance()->shutdown();
}

void GameScene::onGotField(const Field& field, bool myMove, bool iAmWhite)
{
    qDebug() << "GameScene::onGotField" << endl;
    // a verification. It should be the first in this method,
    // because user mustn't see an empty field anyway.
    if(Global::isFieldEmpty(field))
    {
        return;
    }

    MainWindow::instance()->showGameScene();

    mBoard.updateGameField(field, iAmWhite);

    GameState state = UI::instance()->updateGameState(myMove, iAmWhite);
    updateMoveBoxes(state);

    MainWindow::instance()->setMode(MW_NORMAL);
}

void GameScene::update()
{
    GameState state = UI::instance()->getGameState();
    updateMoveBoxes(state);
}

void GameScene::updateItemsPositions(OrientationStatus orientation)
{
    // TODO copy-paste!
    QString orientNode = (orientation == OrientationHorizontal) ? XML_NODE_LANDSCAPE : XML_NODE_PORTRAIT;

    // scene
    int scene_x      = XML::instance().readValue(XML_ITEMS_FILENAME, QList<QString>() << XML_NODE_SCENE << orientNode << XML_NODE_X).toInt();
    int scene_y      = XML::instance().readValue(XML_ITEMS_FILENAME, QList<QString>() << XML_NODE_SCENE << orientNode << XML_NODE_Y).toInt();
    int scene_width  = XML::instance().readValue(XML_ITEMS_FILENAME, QList<QString>() << XML_NODE_SCENE << orientNode << XML_NODE_WIDTH).toInt();
    int scene_height = XML::instance().readValue(XML_ITEMS_FILENAME, QList<QString>() << XML_NODE_SCENE << orientNode << XML_NODE_HEIGHT).toInt();
    setSceneRect( scene_x, scene_y, scene_width, scene_height );
   // addRect( scene_x, scene_y, scene_width, scene_height, QPen(QColor(255, 255, 255)) );

    // border
    mMenuButton->updatePos(orientation);
    mExitButton->updatePos(orientation);


    // chat
    if(mChat)
        mChat->updatePos(orientation);
}



void GameScene::updateMoveBoxes(GameState gameState)
{
    mMeMoveBox.setPlayer(UI::instance()->getPlayer(PT_ME));
    mOppMoveBox.setPlayer(UI::instance()->getPlayer(PT_OPPONENT));

    qDebug() << "GameScene::updateMoveBoxes " << gameState << endl;

    switch(gameState)
    {
        case GS_WAIT_FOR_PLAYER_TOUCH:
        case GS_WAIT_FOR_PLAYER_MOVE:
        case GS_WAIT_FOR_SERVER:
        case GS_INVALID_MOVE:
            mMeMoveBox.setActive();
            mOppMoveBox.setInactive();
            break;
        case GS_WAIT_FOR_OPPONENT:
            mMeMoveBox.setInactive();
            mOppMoveBox.setActive();
            break;
        case GS_GAME_OVER:
            mMeMoveBox.setInactive();
            mOppMoveBox.setInactive();
            break;
        default:
            break;
    }

}





void GameScene::onMenuButtonClicked()
{

    //qDebug() << "onMenuButtonClicked";
    MainWindow::instance()->showGameDialog();
}



