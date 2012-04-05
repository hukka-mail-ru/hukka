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

    // scene
    int scene_x      = XML::instance().readValue(XML_ITEMS_FILENAME, QList<QString>() << XML_NODE_SCENE << XML_NODE_X).toInt();
    int scene_y      = XML::instance().readValue(XML_ITEMS_FILENAME, QList<QString>() << XML_NODE_SCENE << XML_NODE_Y).toInt();
    int scene_width  = XML::instance().readValue(XML_ITEMS_FILENAME, QList<QString>() << XML_NODE_SCENE << XML_NODE_WIDTH).toInt();
    int scene_height = XML::instance().readValue(XML_ITEMS_FILENAME, QList<QString>() << XML_NODE_SCENE << XML_NODE_HEIGHT).toInt();
    setSceneRect( scene_x, scene_y, scene_width, scene_height );

    // chat backgrond
    mChatBackground = new QGraphicsPixmapItem(Pixmaps::get(PIX_CHAT_BACKGROUND));
    this->addItem(mChatBackground);
    mChatBackground->setZValue(Z_BUTTONS_LAYER);
    int x  = XML::instance().readValue(XML_ITEMS_FILENAME, QList<QString>() << XML_NODE_TABLE_CHAT << XML_NODE_X).toInt();
    int y  = XML::instance().readValue(XML_ITEMS_FILENAME, QList<QString>() << XML_NODE_TABLE_CHAT << XML_NODE_Y).toInt();
    mChatBackground->setPos(x, y);

    //chat
    mChat = new Chat(MainWindow::instance(), CT_TABLE_CHAT);
    mChatButton = new Button(this, Pixmaps::get(PIX_BUTTON_CHAT), tr("CHAT"), XML_NODE_BUTTONS, XML_NODE_CHAT);
    QObject::connect(mChatButton, SIGNAL(clicked()), this, SLOT(onChatButtonClicked()));


    // game menu
    mGameMenuButton = new Button(this, Pixmaps::get(PIX_BUTTON_MENU), tr("MENU"), XML_NODE_BUTTONS, XML_NODE_GAME_MENU);
    QObject::connect(mGameMenuButton, SIGNAL(clicked()), this, SLOT(onMenuButtonClicked()));

    connect(Client::instance(), SIGNAL(gotPosition(const Position&)), this, SLOT(onGotPosition(const Position&)));
    connect(Client::instance(), SIGNAL(invalidMove()), this, SLOT(onInvalidMove()));
}



void GameScene::showChat()
{
	if(mChat)
	{
		mChat->show();
	}
}

void GameScene::closeChat()
{
	if(mChat)
	{
		mChat->close();
	}
}


void GameScene::close()
{
	closeChat();
}


void GameScene::onChatButtonClicked()
{
	if(mChat)
	{
		if(mChat->getState() == CHAT_HIDDEN)
		{
			mChat->show();
		}
		else if(mChat->getState() == CHAT_OPEN)
		{
			mChat->hide();
		}
	}
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

// "Check!" is shown only if I'm being checked.
//    if((position.status == Check && position.w_check && position.iAmWhite) ||
//       (position.status == Check && position.b_check && !position.iAmWhite))

    // "Check!" is shown if either I'm checking or I'm being checked.
    if ((position.w_check || position.b_check) && position.status == Check)
    {
        gameState = tr("Check!");
    }
    else if ((position.w_check || position.b_check) && position.status == Checkmate)
    {
        gameState = tr("Mate.");
    }
    else if (position.status == Stalemate)
    {
    	gameState = tr("Stalemate.");
    }
    mMoveBox.setGameStateText(gameState);


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



