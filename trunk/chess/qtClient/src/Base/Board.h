/*
 * Board.h
 *
 *  Created on: Mar 30, 2011
 *      Author: ssy
 */

#ifndef BOARD_H_
#define BOARD_H_

#include <QObject>
#include <QGraphicsScene>
#include <QList>

#include <Global.h>
#include <Animation.h>
#include <CaptureBox.h>
#include <Cell.h>

class Board: public QObject
{
Q_OBJECT
public:
    Board(QGraphicsScene* parentScene);
    virtual ~Board();

    void updatePosition();
    void updatePosition(const Position& position);

    // prevents artifacts
    void repaintCells();

    void disableAnimation();
    void removeHighlight();

private:

    bool isEnemyPiece(CELLID cell);


    void highlightCell(CELLID cell, HighlightColor color = HC_WHITE);
    void enableAnimation(const Move& move);

    QGraphicsScene* mParentScene;

    Position mPosition;

    QGraphicsRectItem* mBoardRect;
    int mBoardRectX;
    int mBoardRectY;

    QGraphicsItem* mCells;  // group of cells
    QList <Cell*> mCellArray;

    QGraphicsItem* mPieces; // group of pieces
    QGraphicsItem* mHighlights; // group of Highlights

    Animation mAnimation;

    CaptureBox mCaptureBox;

    CELLID mHighlightedCell;

    Move mMove;

private slots:
    void onCellClicked(CELLID cell);
};

#endif /* BOARD_H_ */
