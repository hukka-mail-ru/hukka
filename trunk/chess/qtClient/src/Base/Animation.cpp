/*
 * Animation.cpp
 *
 *  Created on: Mar 28, 2011
 *      Author: ssy
 */

#include <XML.h>

#include <MainWindow.h>
#include "Animation.h"

Animation::Animation(GameScene* scene):
    mParentScene(scene)
{
    mTimer = new QTimer(this);
    connect(mTimer, SIGNAL(timeout()), this, SLOT(onTick()));

    mTickDelay = XML::instance().readValue(XML_ITEMS_FILENAME, QList<QString>() << XML_NODE_ANIMATION << XML_NODE_TIMER).toInt();
}

Animation::~Animation()
{
    // TODO Auto-generated destructor stub
}

void Animation::startBlinking(Cell* srcCell, Cell* dstCell)
{
    mTimer->start(mTickDelay);

    mSrcCell = srcCell;
    mDstCell = dstCell;

    mDstNativePiece = mDstCell->getPiece();

    onTick();
}

void Animation::stopBlinking()
{
    mTimer->stop();
}

void Animation::onTick()
{
    static bool odd_tick = false;

    if(odd_tick)
    {
        mSrcCell->showPiece();
        if(mDstNativePiece == Empty)
        {
            mDstCell->hidePiece();
        }
        else
        {
            mDstCell->setPiece(mDstNativePiece);
            mDstCell->highlight();
        }
    }
    else
    {
        mSrcCell->hidePiece();
        mDstCell->setPiece(mSrcCell->getPiece());
        mDstCell->highlight();
    }

    // prevents artifacts
    mParentScene->repaintCells();

    odd_tick = !odd_tick;
}
