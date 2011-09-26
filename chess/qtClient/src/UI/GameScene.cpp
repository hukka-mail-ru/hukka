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
    mMoveBox(this),
    mBoard(this)
{

    // GameState (check, mate, etc..)
    int gameStateX           = XML::instance().readValue(XML_ITEMS_FILENAME, QList<QString>() << XML_NODE_GAME_STATE << XML_NODE_X).toInt();
    int gameStateY           = XML::instance().readValue(XML_ITEMS_FILENAME, QList<QString>() << XML_NODE_GAME_STATE << XML_NODE_Y).toInt();
    QString gameStateFamily  = XML::instance().readValue(XML_ITEMS_FILENAME, QList<QString>() << XML_NODE_GAME_STATE << XML_NODE_FONT << XML_NODE_FAMILY);
    int gameStateSize        = XML::instance().readValue(XML_ITEMS_FILENAME, QList<QString>() << XML_NODE_GAME_STATE << XML_NODE_FONT << XML_NODE_SIZE).toInt();
    QString gameStateColor   = XML::instance().readValue(XML_ITEMS_FILENAME, QList<QString>() << XML_NODE_GAME_STATE << XML_NODE_FONT << XML_NODE_COLOR);
    mGameStateText = this->addText("", QFont(gameStateFamily, gameStateSize));
    mGameStateText->setDefaultTextColor(gameStateColor);
    mGameStateText->setPos(gameStateX, gameStateY);

    // scene
    int scene_x      = XML::instance().readValue(XML_ITEMS_FILENAME, QList<QString>() << XML_NODE_SCENE << XML_NODE_X).toInt();
    int scene_y      = XML::instance().readValue(XML_ITEMS_FILENAME, QList<QString>() << XML_NODE_SCENE << XML_NODE_Y).toInt();
    int scene_width  = XML::instance().readValue(XML_ITEMS_FILENAME, QList<QString>() << XML_NODE_SCENE << XML_NODE_WIDTH).toInt();
    int scene_height = XML::instance().readValue(XML_ITEMS_FILENAME, QList<QString>() << XML_NODE_SCENE << XML_NODE_HEIGHT).toInt();
    setSceneRect( scene_x, scene_y, scene_width, scene_height );

    // game menu
    mGameMenuButton = new Button(this, Pixmaps::get(PIX_BUTTON_MENU), tr("Game menu"), XML_NODE_BUTTONS, XML_NODE_GAME_MENU);
    QObject::connect(mGameMenuButton, SIGNAL(clicked()), this, SLOT(onMenuButtonClicked()));

    connect(Client::instance(), SIGNAL(gotPosition(const Position&)), this, SLOT(onGotPosition(const Position&)));
    connect(Client::instance(), SIGNAL(invalidMove()), this, SLOT(onInvalidMove()));
}



void GameScene::showChat()
{
    mChat = new Chat(MainWindow::instance(), CT_TABLE_CHAT);
    //this->addWidget(mChat);
    mChat->show();
}


void GameScene::close()
{
   if(mChat)
       mChat->close();
}


void GameScene::onInvalidMove()
{
    MainWindow::instance()->showMessage(tr("Invalid move."));
    UI::instance()->setGameState(GS_WAIT_FOR_PLAYER_TOUCH);

    mBoard.removeHighlight();
    mBoard.updatePosition();
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

/*
void GameScene::onExitClicked()
{
    close();
    UI::instance()->shutdown();
}*/

void GameScene::onGotPosition(const Position& position)
{
    if(UI::instance()->getGameState() == GS_GAME_OVER )
        return;

//    qDebug() << "GameScene::onGotField" << endl;
    // a verification. It should be the first in this method,
    // because user mustn't see an empty field anyway.
    if(Global::isFieldEmpty(position.field))
    {
        return;
    }

    MainWindow::instance()->showGameScene();

    mBoard.updatePosition(position);

    GameState state = UI::instance()->updateGameState(position.myMove, position.iAmWhite);
    updateMoveBoxes(state);

    // Show game state
    QString gameState = "";
    if((position.status == Check && position.w_check && position.iAmWhite) ||
       (position.status == Check && position.b_check && !position.iAmWhite))
    {
        gameState = tr("Check!");
    }
    else if ((position.w_check || position.b_check) && position.status == Checkmate)
    {
        gameState = tr("Mate.");
    }
    else if (position.status == Stalemate)
    {
        mGameStateText->setPlainText(tr("Stalemate."));
    }
    mGameStateText->setPlainText(gameState);


    MainWindow::instance()->setMode(MW_NORMAL);
}

void GameScene::update()
{
    GameState state = UI::instance()->getGameState();
    updateMoveBoxes(state);
}



void GameScene::updateMoveBoxes(GameState gameState)
{

    switch(gameState)
    {
        case GS_WAIT_FOR_PLAYER_TOUCH:
        case GS_WAIT_FOR_PLAYER_MOVE:
        case GS_WAIT_FOR_SERVER:
        case GS_INVALID_MOVE:
            mMoveBox.setPlayer(PT_ME);
            mMoveBox.setActive();
            break;
        case GS_WAIT_FOR_OPPONENT:
            mMoveBox.setPlayer(PT_OPPONENT);
            mMoveBox.setInactive();
            break;
        case GS_GAME_OVER:
            mMoveBox.setInactive();
            mMoveBox.setGameOver();
            break;
        default:
            break;
    }

}



void GameScene::onMenuButtonClicked()
{
    MainWindow::instance()->showGameDialog(tr("Game menu"));
}



