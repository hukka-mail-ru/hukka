/*
 * Cell.cpp
 *
 *  Created on: May 20, 2010
 *      Author: ssy
 */


#include <QPainter>
#include <QDebug>
#include <UI.h>
#include "Cell.h"

Cell::Cell(QGraphicsScene* scene, CELLID cellID, PixmapKey cellPixmapKey, QObject* parent):
    Button(scene, Pixmaps::get(cellPixmapKey), "", "", ""),
    mId(cellID),
    mCellPixmapKey(cellPixmapKey),
    mPiecePixmapKey(PIX_NONE),
    mIsPiece(false),
    mIsHighlight(false),
    mPieceType(Empty)
{
    mHighlight = PIX_CELL_WHITE_HIGHLIGHT;
}

void Cell::setPiece(piece_type pieceType)
{
    PixmapKey piecePixmapKey = PIX_NONE;
    mPieceType = pieceType;

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
    mIsPiece = true;
}


void Cell::showPiece()
{
    applyPixmap(mCellPixmapKey);
    applyPixmap(mPiecePixmapKey);
    if(mIsHighlight)
    {
        applyPixmap(mHighlight);
    }

    mIsPiece = true;
}

void Cell::hidePiece()
{
    applyPixmap(mCellPixmapKey);
    if(mIsHighlight)
    {
        applyPixmap(mHighlight);
    }

    mIsPiece = false;
}

void Cell::highlight(HighlightColor color)
{
    mHighlight = (color == HC_WHITE) ? PIX_CELL_WHITE_HIGHLIGHT : PIX_CELL_GRAY_HIGHLIGHT;

    applyPixmap(mHighlight);
    mIsHighlight = true;
}

void Cell::removeHighlight()
{
    applyPixmap(mCellPixmapKey);
    if(mIsPiece)
    {
        applyPixmap(mPiecePixmapKey);
    }

    mIsHighlight = false;
}



void Cell::applyPixmap(PixmapKey key)
{
    if(key == PIX_NONE)
        return;

    QPixmap srcPixmap = Pixmaps::get(key);
    QPixmap dstPixmap = this->pixmap();

   // int middle = (dstPixmap.width() - srcPixmap.width())/2;

    QRect dst(0, 0, srcPixmap.width(), srcPixmap.height());
    QRect src(0, 0, dstPixmap.width(), dstPixmap.height());

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
