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
       mBoard(NULL), mCells(NULL), mPieces(NULL), mHighlights(NULL),  mGameStateText(NULL),
       mChat(NULL),
       mMoveClock(this, tr("Move: "), SIGNAL(gotMoveTime(quint32)), XML_NODE_MOVE_CLOCK),
       mGameClock(this, tr("Game: "), SIGNAL(gotGameTime(quint32)), XML_NODE_GAME_CLOCK),
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

void GameScene::close()
{
   if(mChat && mChat->isVisible())
       mChat->close();
}

void GameScene::initialize()
{

    // BOARD
    QString border_color =  XML::instance().readValue(XML_ITEMS_FILENAME, QList<QString>() << XML_NODE_BOARD << XML_NODE_BORDER << XML_NODE_COLOR);
    //mBoard = addRect (0, 0, 0, 0, QPen(QColor(border_color)));

    // MENU BUTTON
    mMenuButton = new Button(this, Pixmaps::get(PIX_BUTTON_MENU), tr("Game menu"), XML_NODE_GAME_MENU);
   // mMenuButton->updatePos(MainWindow::instance()->getOrientation());
    QObject::connect(mMenuButton, SIGNAL(clicked()), this, SLOT(onMenuButtonClicked()));

    mExitButton = new Button(this, Pixmaps::get(PIX_BUTTON_EXIT), "", XML_NODE_EXIT);
    QObject::connect(mExitButton, SIGNAL(clicked()), this, SLOT(onExitClicked()));


    QString family = XML::instance().readValue(XML_ITEMS_FILENAME, QList<QString>() << XML_NODE_GAME_STATE_TEXT << XML_NODE_FONT << XML_NODE_FAMILY);
    int size =       XML::instance().readValue(XML_ITEMS_FILENAME, QList<QString>() << XML_NODE_GAME_STATE_TEXT << XML_NODE_FONT << XML_NODE_SIZE).toInt();
    mTextActiveColor   =  XML::instance().readValue(XML_ITEMS_FILENAME, QList<QString>() << XML_NODE_GAME_STATE_TEXT << XML_NODE_FONT << XML_NODE_COLOR << XML_NODE_ACTIVE);
    mTextInactiveColor =  XML::instance().readValue(XML_ITEMS_FILENAME, QList<QString>() << XML_NODE_GAME_STATE_TEXT << XML_NODE_FONT << XML_NODE_COLOR << XML_NODE_INACTIVE);

    mGameStateText = addText("", QFont(family, size));
    mGameStateText->setDefaultTextColor( QColor(mTextActiveColor) );


    connect(Client::instance(), SIGNAL(gotField(Field, bool, bool)), this, SLOT(onGotField(Field, bool, bool)));

}

void GameScene::onExitClicked()
{
    close();
    UI::instance()->shutdown();
}

void GameScene::onGotField(const Field& field, bool myMove, bool iAmWhite)
{
    updateGameField(field, iAmWhite);
    MainWindow::instance()->setMode(MW_NORMAL);

    setGameStateText(UI::instance()->updateField(field, myMove, iAmWhite));

    updateClocks();
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
    int board_x = XML::instance().readValue(XML_ITEMS_FILENAME, QList<QString>() << XML_NODE_BOARD << orientNode << XML_NODE_X).toInt();
    int board_y = XML::instance().readValue(XML_ITEMS_FILENAME, QList<QString>() << XML_NODE_BOARD << orientNode << XML_NODE_Y).toInt();
    int board_width = XML::instance().readValue(XML_ITEMS_FILENAME, QList<QString>() << XML_NODE_BOARD << XML_NODE_WIDTH).toInt();

    mBoard = addRect (board_x, board_y, board_width,  board_width, QPen(QColor(0, 0, 0)));
    updateGameField(mField, mWhite);

    // clocks
    mMoveClock.updatePos(orientation);
    mGameClock.updatePos(orientation);

    // text
    mTextX = XML::instance().readValue(XML_ITEMS_FILENAME, QList<QString>() << XML_NODE_GAME_STATE_TEXT << orientNode << XML_NODE_X).toInt();
    mTextY = XML::instance().readValue(XML_ITEMS_FILENAME, QList<QString>() << XML_NODE_GAME_STATE_TEXT << orientNode << XML_NODE_Y).toInt();
    mTextFrameWidth = XML::instance().readValue(XML_ITEMS_FILENAME, QList<QString>() << XML_NODE_GAME_STATE_TEXT << orientNode << XML_NODE_WIDTH).toInt();


    // chat
    if(mChat)
        mChat->updatePos(orientation);
}


void GameScene::startClocks()
{

    mMoveClock.start();
    mGameClock.start();
}

void GameScene::updateClocks()
{
    mMoveClock.getServerTime();
}


void GameScene::setGameStateText(GameState gameState)
{
    QString text;
    QString color;

    switch(gameState)
    {
        case GS_WAIT_FOR_PLAYER_TOUCH:
        case GS_WAIT_FOR_PLAYER_MOVE:
        case GS_WAIT_FOR_SERVER:
        case GS_INVALID_MOVE:       text = tr("Your move");       color = mTextActiveColor;  break;
        case GS_WAIT_FOR_OPPONENT:  text = tr("Opponent's move"); color = mTextInactiveColor; break;
        default: break;
    }

    mGameStateText->setPlainText(text);

    // center alignment
    mGameStateText->setPos(mTextX + (mTextFrameWidth - mGameStateText->boundingRect().width())/2 , mTextY);

    mGameStateText->setDefaultTextColor(QColor(color));

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

        if((i * CELLS_IN_ROW + i + j) % 2) // odd-even cells
        {
            cell = new Cell(this, cellID, PIX_CELL_WHITE);
        }
        else
        {
            cell = new Cell(this, cellID, PIX_CELL_BLACK);
        }


        if(cell && !field.empty())
            cell->setPiece(field[i * CELLS_IN_ROW + j]);

        QObject::connect(cell, SIGNAL(cellClicked(CELLID)), this, SLOT(onCellClicked(CELLID)));

        mCellArray.push_back(cell); // memorize the pointer
        cell->setZValue(Z_CELLS_LAYER);
        cell->setParentItem(mCells);

        if(white)
        {
            cell->moveBy(mBoard->rect().x() + j * Cell::width(), mBoard->rect().y()  + (CELLS_IN_ROW - 1 - i) * Cell::width());
        }
        else
        {
            cell->moveBy(mBoard->rect().x() + j * Cell::width(), mBoard->rect().y() + i * Cell::width());
        }
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


