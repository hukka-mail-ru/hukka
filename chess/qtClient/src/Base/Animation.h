/*
 * Animation.h
 *
 *  Created on: Mar 28, 2011
 *      Author: ssy
 */

#ifndef ANIMATION_H_
#define ANIMATION_H_

#include <QObject>
#include <QTimer>
#include <Cell.h>

class Board;

class Animation: public QObject
{
    Q_OBJECT
public:
    Animation(Board* parentBoard);

    void startBlinking(Cell* srcCell, Cell* dstCell);
    void stopBlinking();

    virtual ~Animation();

private:

    Board* mParentBoard;
    QTimer* mTimer;
    int mTickDelay;

    Cell* mSrcCell;
    Cell* mDstCell;

    piece_type mDstNativePiece;

private slots:

    void onTick();
};

#endif /* ANIMATION_H_ */
