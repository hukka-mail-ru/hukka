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

class Animation: public QObject
{
    Q_OBJECT
public:
    Animation();

    void startBlinking(Cell* srcCell, Cell* dstCell);
    void stopBlinking();

    virtual ~Animation();

private:

    QTimer* mTimer;
    int mTickDelay;

    Cell* mSrcCell;
    Cell* mDstCell;


private slots:

    void onTick();
};

#endif /* ANIMATION_H_ */
