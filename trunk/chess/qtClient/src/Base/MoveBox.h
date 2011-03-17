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

#include <Defines.h>
#include <Player.h>

class MoveBox: public QObject
{
Q_OBJECT
public:
    MoveBox(QGraphicsScene* parentScene, PlayerType playerType);
    virtual ~MoveBox();

    // change color and "your move" -> "opponent's move"
    void setActive();
    void setInactive();

    void setPlayer(const Player& player);

private:

    QGraphicsRectItem* mBorder;

    // childs of mBorder
    //QGraphicsTextItem* mMoveText;
    QGraphicsTextItem* mPlayerNameText;
    QGraphicsTextItem* mRatingText;

    // Clock mMoveClock;

    QColor mActiveColor;
    QColor mInactiveColor;

};

#endif /* MOVEBOX_H_ */
