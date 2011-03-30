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
#include <Defines.h>
#include <Animation.h>
#include <CaptureBox.h>
#include <Cell.h>

class Board: public QObject
{
Q_OBJECT
public:
    Board(QGraphicsScene* parentScene);
    virtual ~Board();

    void updateGameField(const Field& field, bool white);

    // prevents artifacts
    void repaintCells();

private:

    bool isEnemyPiece(CELLID cell);


    void highlightCell(CELLID cell);
    void removeHighlight();
    void enableAnimation(const Move& move);
    void disableAnimation();

    QGraphicsScene* mParentScene;

    Field mField;

    QGraphicsRectItem* mBoardRect;
    int mBoardRectX;
    int mBoardRectY;

    QGraphicsItem* mCells;  // group of cells
    vector <Cell*> mCellArray;

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
