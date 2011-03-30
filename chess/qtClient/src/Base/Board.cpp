/*
 * Board.cpp
 *
 *  Created on: Mar 30, 2011
 *      Author: ssy
 */

#include <UI.h>
#include <Client.h>
#include <XML.h>
#include <MainWindow.h>
#include "Board.h"


Board::Board(QGraphicsScene* parentScene):
    QObject(parentScene),
    mParentScene(parentScene),
    mBoardRect(NULL),
    mCells(NULL),
    mPieces(NULL),
    mHighlights(NULL),
    mAnimation(this),
    mCaptureBox(parentScene)
{
    // BORDER
    //QString border_color =  XML::instance().readValue(XML_ITEMS_FILENAME, QList<QString>() << XML_NODE_BOARD << XML_NODE_BORDER << XML_NODE_COLOR);
    //mBoard = addRect (0, 0, 0, 0, QPen(QColor(border_color)));

    // board and field
    mBoardRectX = XML::instance().readValue(XML_ITEMS_FILENAME, QList<QString>() << XML_NODE_BOARD  << XML_NODE_X).toInt();
    mBoardRectY = XML::instance().readValue(XML_ITEMS_FILENAME, QList<QString>() << XML_NODE_BOARD  << XML_NODE_Y).toInt();
    int board_width = XML::instance().readValue(XML_ITEMS_FILENAME, QList<QString>() << XML_NODE_BOARD << XML_NODE_WIDTH).toInt();

    mBoardRect = mParentScene->addRect (mBoardRectX, mBoardRectY, board_width,  board_width, QPen(QColor(0, 0, 0)));
   // updateGameField(mField, mWhite);

}

Board::~Board() {
    // TODO Auto-generated destructor stub
}

bool Board::isEnemyPiece(CELLID cell)
{
    bool res = false;
    if(UI::instance()->getPlayer(PT_ME).color == PC_WHITE)
    {
        // qDebug() <<"isEnemyPiece" << cell << (mField.cells[cell] == PIX_BLACK_ROOK);

        res = (mField[cell] == b_Pawn) ||
              (mField[cell] == b_Rook) ||
              (mField[cell] == b_Knight) ||
              (mField[cell] == b_Bishop) ||
              (mField[cell] == b_Queen) ||
              (mField[cell] == b_King);
    }
    else
    {
        res = (mField[cell] == w_Pawn) ||
              (mField[cell] == w_Rook) ||
              (mField[cell] == w_Knight) ||
              (mField[cell] == w_Bishop) ||
              (mField[cell] == w_Queen) ||
              (mField[cell] == w_King);
    }

    return res;
}

piece_type getPromotion(piece_type piece, CELLID dstCell)
{
    piece_type res = Empty;

    if(piece != w_Pawn && piece != b_Pawn)
        return res;

    if(piece == w_Pawn && dstCell >= (CELLS_IN_FIELD - CELLS_IN_ROW)) // the last line for whites
    {
 //       qDebug() << "getPromotion 1";
        res = MainWindow::instance()->showPromotionDialog(PC_WHITE);
    }
    else if(piece == b_Pawn && dstCell < CELLS_IN_ROW) // the last line for blacks
    {
 //       qDebug() << "getPromotion 2";
        res = MainWindow::instance()->showPromotionDialog(PC_BLACK);
    }

  //  qDebug() << "getPromotion 3";

    return res;
}



void Board::onCellClicked(CELLID cell)
{


    switch(UI::instance()->getGameState())
    {
    case GS_WAIT_FOR_SERVER:

        // qDebug() << "No action - GS_WAIT_FOR_SERVER";
        return;

    case GS_WAIT_FOR_OPPONENT:

        // qDebug() << "No action - GS_WAIT_FOR_OPPONENT";
        return;

    case GS_WAIT_FOR_PLAYER_TOUCH:

        if(mField[cell] == Empty || isEnemyPiece(cell))
        {
            // qDebug() << "No action - Epmty field or Enemy piece";
            return;
        }
        else
        {
            mMove.srcCell = cell;

            highlightCell(mMove.srcCell);
            UI::instance()->setGameState(GS_WAIT_FOR_PLAYER_MOVE);
            // qDebug() << "OK! mSourceCell = " << cell;
        }

        break;

    case GS_WAIT_FOR_PLAYER_MOVE:

        if(mField[cell] == Empty || isEnemyPiece(cell)) // I've moved my piece
        {
            mMove.dstCell = cell;
            // qDebug() << "OK! mDestinationCell = " << cell;

            removeHighlight();
            enableAnimation(mMove);

         //   qDebug() << "mField[move.srcCell]" << mField[move.srcCell];
         //   qDebug() << "move.dstCell" << move.dstCell;

            piece_type promotion = getPromotion(mField[mMove.srcCell], mMove.dstCell);

            Client::instance()->move(UI::instance()->getGameTable(), mMove, promotion);

            UI::instance()->setGameState(GS_WAIT_FOR_SERVER);
        }
        else // I've changed my mind and selected another piece.
        {
            mMove.srcCell = cell;

            removeHighlight();
            highlightCell(mMove.srcCell);

            UI::instance()->setGameState(GS_WAIT_FOR_PLAYER_MOVE);
            // qDebug() << "OK! mSourceCell = " << cell;
        }

        break;

    default:
        break;
    }

}


void Board::updateGameField(const Field& field, bool white)
{
    disableAnimation();

    if(mField == field)
    {
        MainWindow::instance()->showMessage(tr("Invalid move."));
    }

    mField = field;


    if(mCells)
    {
        mParentScene->removeItem(mCells);
        mCellArray.clear();
    }

    mCells = new QGraphicsPixmapItem(mBoardRect);
    mCells->setZValue(Z_CELLS_LAYER);

    for(int i=0; i<CELLS_IN_ROW; ++i) // Rows
    for(int j=0; j<CELLS_IN_ROW; ++j) // columns
    {

        Cell* cell = 0;
        CELLID cellID = i * CELLS_IN_ROW + j;

        // odd-even cells
        PixmapKey cellKey = ((i * CELLS_IN_ROW + i + j) % 2) ? PIX_CELL_WHITE : PIX_CELL_BLACK;
        cell = new Cell(mParentScene, cellID, cellKey);


        if(cell && !field.empty())
            cell->setPiece(field[i * CELLS_IN_ROW + j]);

        QObject::connect(cell, SIGNAL(cellClicked(CELLID)), this, SLOT(onCellClicked(CELLID)));

        mCellArray.push_back(cell); // memorize the pointer
        cell->setZValue(Z_CELLS_LAYER);
        cell->setParentItem(mCells);

        int x = mBoardRectX + j * cell->width();
        int y = (white) ? mBoardRectY  + (CELLS_IN_ROW - 1 - i) * cell->width() :
                          mBoardRectY + i * cell->width();

        cell->setPos(x, y);
    }

    // show Captured pieces
    mCaptureBox.update(field, white);


}


void Board::highlightCell(CELLID cell)
{
    mCellArray[cell]->highlight();
    mHighlightedCell = cell;
}

void Board::removeHighlight()
{
    mCellArray[mHighlightedCell]->removeHighlight();
}


void Board::enableAnimation(const Move& move)
{
    Cell* srcCell = mCellArray[move.srcCell];
    Cell* dstCell = mCellArray[move.dstCell];

    mAnimation.startBlinking(srcCell, dstCell);
}

void Board::disableAnimation()
{
    mAnimation.stopBlinking();
}

void Board::repaintCells()
{
    for(int i =0; i<mCellArray.size(); i++)
        mCellArray[i]->update();
}
