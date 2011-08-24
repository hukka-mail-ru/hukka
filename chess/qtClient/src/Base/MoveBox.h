/*
 * MoveBox.h
 *
 *  Created on: Mar 17, 2011
 *      Author: ssy
 */

#ifndef MOVEBOX_H_
#define MOVEBOX_H_

#include <QGraphicsScene>
#include <QGraphicsRectItem>
#include <QGraphicsTextItem>
#include <QString>
#include <QColor>
#include <QObject>
#include <Clock.h>

#include <Defines.h>

class MoveBox
{
public:
    MoveBox(QGraphicsScene* parentScene);
    virtual ~MoveBox();

    // change color and "your move" -> "opponent's move"
    void setActive();
    void setInactive();

    void setPlayer(const Player& player);

    void setGameOver();


private:

    QGraphicsRectItem* mBorder;

    // childs of mBorder
    //QGraphicsTextItem* mMoveText;
    QGraphicsTextItem* mPlayerNameText;
  //  QGraphicsTextItem* mRatingText;

    // Clock mMoveClock;

    QColor mActiveColor;
    QColor mInactiveColor;

    Clock mMoveClock;
    Clock mGameClock;

};

#endif /* MOVEBOX_H_ */
