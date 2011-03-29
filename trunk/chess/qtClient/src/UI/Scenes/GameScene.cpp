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
       mParent(parent),
       mBoard(NULL), mCells(NULL), mPieces(NULL), mHighlights(NULL),
       mChat(NULL),
       mMeMoveBox(this, PT_ME),
       mOppMoveBox(this, PT_OPPONENT),
       mCaptureBox(this)
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

void GameScene::startClocks()
{
  //  qDebug() << "GameScene::startClocks";

    mMeMoveBox.startClocks();
    mOppMoveBox.startClocks();
}

void GameScene::close()
{
   if(mChat)
       mChat->close();
}

void GameScene::initialize()
{

    // BOARD
    QString border_color =  XML::instance().readValue(XML_ITEMS_FILENAME, QList<QString>() << XML_NODE_BOARD << XML_NODE_BORDER << XML_NODE_COLOR);
    //mBoard = addRect (0, 0, 0, 0, QPen(QColor(border_color)));

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
    disableAnimation();

    MainWindow::instance()->showGameScene(UI::instance()->getPlayer(PT_ME).color);

    if(mField == field)
    {
        MainWindow::instance()->showMessage(tr("Invalid move."));
    }

    updateGameField(field, iAmWhite);
    updateMoveBoxes(UI::instance()->updateField(field, myMove, iAmWhite));

    if(!Global::isFieldEmpty(field))
    {
        MainWindow::instance()->setMode(MW_NORMAL);
    }
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

    // board and field
    mBoardX = XML::instance().readValue(XML_ITEMS_FILENAME, QList<QString>() << XML_NODE_BOARD << orientNode << XML_NODE_X).toInt();
    mBoardY = XML::instance().readValue(XML_ITEMS_FILENAME, QList<QString>() << XML_NODE_BOARD << orientNode << XML_NODE_Y).toInt();
    int board_width = XML::instance().readValue(XML_ITEMS_FILENAME, QList<QString>() << XML_NODE_BOARD << XML_NODE_WIDTH).toInt();

    mBoard = addRect (mBoardX, mBoardY, board_width,  board_width, QPen(QColor(0, 0, 0)));
    updateGameField(mField, mWhite);


    // chat
    if(mChat)
        mChat->updatePos(orientation);
}



void GameScene::updateMoveBoxes(GameState gameState)
{
    mMeMoveBox.setPlayer(UI::instance()->getPlayer(PT_ME));
    mOppMoveBox.setPlayer(UI::instance()->getPlayer(PT_OPPONENT));

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
        default:
            break;
    }

}

void GameScene::updateGameField(const Field& field, bool white)
{
    mField = field;
    mWhite = white;


    if(mCells)
    {
        removeItem(mCells);
        mCellArray.clear();
    }

    mCells = new QGraphicsPixmapItem(mBoard);
    mCells->setZValue(Z_CELLS_LAYER);

    for(int i=0; i<CELLS_IN_ROW; ++i) // Rows
    for(int j=0; j<CELLS_IN_ROW; ++j) // columns
    {

        Cell* cell = 0;
        CELLID cellID = i * CELLS_IN_ROW + j;

        // odd-even cells
        PixmapKey cellKey = ((i * CELLS_IN_ROW + i + j) % 2) ? PIX_CELL_WHITE : PIX_CELL_BLACK;
        cell = new Cell(this, cellID, cellKey);


        if(cell && !field.empty())
            cell->setPiece(field[i * CELLS_IN_ROW + j]);

        QObject::connect(cell, SIGNAL(cellClicked(CELLID)), this, SLOT(onCellClicked(CELLID)));

        mCellArray.push_back(cell); // memorize the pointer
        cell->setZValue(Z_CELLS_LAYER);
        cell->setParentItem(mCells);

        int x = mBoardX + j * cell->width();
        int y = (white) ? mBoardY  + (CELLS_IN_ROW - 1 - i) * cell->width() :
                          mBoardY + i * cell->width();

        cell->setPos(x, y);
    }

    // show Captured pieces
    mCaptureBox.update(field, white);


}


void GameScene::highlightCell(CELLID cell)
{
    mCellArray[cell]->highlight();
    mHighlightedCell = cell;
}

void GameScene::removeHighlight()
{
    mCellArray[mHighlightedCell]->removeHighlight();
}




void GameScene::onCellClicked(CELLID cellID)
{

  //  qDebug() << "onCellClicked: " << cellID;
    UI::instance()->cellClicked(cellID);
}

void GameScene::onMenuButtonClicked()
{

    //qDebug() << "onMenuButtonClicked";
    MainWindow::instance()->showGameDialog();
}

void GameScene::enableAnimation(const Move& move)
{
    Cell* srcCell = mCellArray[move.srcCell];
    Cell* dstCell = mCellArray[move.dstCell];

    mAnimation.startBlinking(srcCell, dstCell);
}

void GameScene::disableAnimation()
{
    mAnimation.stopBlinking();
}

