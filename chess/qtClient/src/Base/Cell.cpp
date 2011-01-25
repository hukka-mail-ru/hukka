/*
 * Cell.cpp
 *
 *  Created on: May 20, 2010
 *      Author: ssy
 */

#include "Cell.h"
#include <QPainter>
#include <QDebug>

int Cell::mWidth = 0;

Cell::Cell(QGraphicsScene* scene, CELLID cellID, PixmapKey cellPixmapKey, QObject* parent):
    Button(scene, Pixmaps::get(cellPixmapKey), "", ""),
    mId(cellID),
    mCellPixmapKey(cellPixmapKey)
{
}

void Cell::setPiece(PixmapKey piecePixmapKey)
{
    applyPixmap(piecePixmapKey);
    mPiecePixmapKey = piecePixmapKey;
}

void Cell::highlight()
{
    applyPixmap(PIX_CELL_HIGHLIGHT);
}

void Cell::removeHighlight()
{
    applyPixmap(mCellPixmapKey);
    applyPixmap(mPiecePixmapKey);
}



void Cell::applyPixmap(PixmapKey key)
{
    if(key == PIX_NONE)
        return;

    QPixmap srcPixmap = Pixmaps::get(key);
    QPixmap dstPixmap = this->pixmap();

    double middle = (dstPixmap.width() - srcPixmap.width())/2;

    QRectF dst(middle, 0.0, srcPixmap.width(), srcPixmap.height());
    QRectF src(0.0, 0.0, srcPixmap.width(), srcPixmap.height());

   // qDebug() << "addPiecePixmap " << dstPixmap.width() << " " << dstPixmap.height() << " " <<
   // srcPixmap.width()<< " " << srcPixmap.height();

    QPainter painter(&dstPixmap);
    painter.drawPixmap(dst, srcPixmap, src);

    this->setPixmap(dstPixmap);
}




void Cell::mousePressEvent (QGraphicsSceneMouseEvent * event)
{
    Button::mousePressEvent(event);
}


void Cell::mouseReleaseEvent (QGraphicsSceneMouseEvent * event)
{
    emit cellClicked(mId);

    Button::mouseReleaseEvent(event);
}
