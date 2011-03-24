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
    Button(scene, Pixmaps::get(cellPixmapKey), "", "", ""),
    mId(cellID),
    mCellPixmapKey(cellPixmapKey)
{
}

void Cell::setPiece(piece_type pieceType)
{
    PixmapKey piecePixmapKey = PIX_NONE;

    if(mCellPixmapKey == PIX_CELL_WHITE)
    {
        switch(pieceType)
        {
            case w_Rook:   piecePixmapKey = PIX_WHITE_ROOK_ON_WHITE; break;
            case w_Knight: piecePixmapKey = PIX_WHITE_KNIGHT_ON_WHITE; break;
            case w_Bishop: piecePixmapKey = PIX_WHITE_BISHOP_ON_WHITE; break;
            case w_Queen:  piecePixmapKey = PIX_WHITE_QUEEN_ON_WHITE; break;
            case w_King:   piecePixmapKey = PIX_WHITE_KING_ON_WHITE; break;
            case w_Pawn:   piecePixmapKey = PIX_WHITE_PAWN_ON_WHITE; break;

            case b_Rook:   piecePixmapKey = PIX_BLACK_ROOK_ON_WHITE; break;
            case b_Knight: piecePixmapKey = PIX_BLACK_KNIGHT_ON_WHITE; break;
            case b_Bishop: piecePixmapKey = PIX_BLACK_BISHOP_ON_WHITE; break;
            case b_Queen:  piecePixmapKey = PIX_BLACK_QUEEN_ON_WHITE; break;
            case b_King:   piecePixmapKey = PIX_BLACK_KING_ON_WHITE; break;
            case b_Pawn:   piecePixmapKey = PIX_BLACK_PAWN_ON_WHITE; break;

            default: break;
        }
    }
    else if(mCellPixmapKey == PIX_CELL_BLACK)
    {
        switch(pieceType)
        {
            case w_Rook:   piecePixmapKey = PIX_WHITE_ROOK_ON_BLACK; break;
            case w_Knight: piecePixmapKey = PIX_WHITE_KNIGHT_ON_BLACK; break;
            case w_Bishop: piecePixmapKey = PIX_WHITE_BISHOP_ON_BLACK; break;
            case w_Queen:  piecePixmapKey = PIX_WHITE_QUEEN_ON_BLACK; break;
            case w_King:   piecePixmapKey = PIX_WHITE_KING_ON_BLACK; break;
            case w_Pawn:   piecePixmapKey = PIX_WHITE_PAWN_ON_BLACK; break;

            case b_Rook:   piecePixmapKey = PIX_BLACK_ROOK_ON_BLACK; break;
            case b_Knight: piecePixmapKey = PIX_BLACK_KNIGHT_ON_BLACK; break;
            case b_Bishop: piecePixmapKey = PIX_BLACK_BISHOP_ON_BLACK; break;
            case b_Queen:  piecePixmapKey = PIX_BLACK_QUEEN_ON_BLACK; break;
            case b_King:   piecePixmapKey = PIX_BLACK_KING_ON_BLACK; break;
            case b_Pawn:   piecePixmapKey = PIX_BLACK_PAWN_ON_BLACK; break;

            default: break;
        }
    }


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
