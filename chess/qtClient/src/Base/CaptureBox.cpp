/*
 * CaptureBox.cpp
 *
 *  Created on: Feb 25, 2011
 *      Author: ssy
 */

#include "CaptureBox.h"

#include <QList>
#include <QDebug>
#include <QGraphicsPixmapItem>
#include <XML.h>


CaptureBox::CaptureBox(QGraphicsScene* parentScene):
    mParentScene(parentScene)
{
    mMeX = XML::instance().readValue(XML_ITEMS_FILENAME, QList<QString>() << XML_NODE_CAPTURED << XML_NODE_ME << XML_NODE_X).toInt();
    mMeY = XML::instance().readValue(XML_ITEMS_FILENAME, QList<QString>() << XML_NODE_CAPTURED << XML_NODE_ME << XML_NODE_Y).toInt();

    mOpponentX = XML::instance().readValue(XML_ITEMS_FILENAME, QList<QString>() << XML_NODE_CAPTURED << XML_NODE_OPPONENT << XML_NODE_X).toInt();
    mOpponentY = XML::instance().readValue(XML_ITEMS_FILENAME, QList<QString>() << XML_NODE_CAPTURED << XML_NODE_OPPONENT << XML_NODE_Y).toInt();

    mWidth  = XML::instance().readValue(XML_ITEMS_FILENAME, QList<QString>() << XML_NODE_CAPTURED << XML_NODE_WIDTH).toInt();

}

CaptureBox::~CaptureBox() {
    // TODO Auto-generated destructor stub
}



void CaptureBox::update(const Field& field, bool white)
{
    if(field.empty())
        return;

    // get all the pieces
    QList<piece_type> white_pieces;
    QList<piece_type> black_pieces;

    white_pieces << w_Queen << w_Rook << w_Rook << w_Knight << w_Knight << w_Bishop << w_Bishop;
    black_pieces << b_Queen << b_Rook << b_Rook << b_Knight << b_Knight << b_Bishop << b_Bishop;

    for(int i=0; i<8; i++)
    {
        white_pieces << w_Pawn;
        black_pieces << b_Pawn;
    }

    // remove all but the captured
    for(int i=0; i<field.size(); i++)
    {
        piece_type piece = field[i];

        if(piece > Empty && piece < Black) // a white piece
        {
            white_pieces.removeOne(piece);
        }
        else if(piece > Black) // a black piece
        {
            black_pieces.removeOne(piece);
        }
    }


    // draw captured whites
    for(int i=0; i<white_pieces.size(); i++)
    {
        PixmapKey piecePixmapKey = PIX_NONE;
        switch(white_pieces[i])
        {
            case w_Rook:   piecePixmapKey = PIX_CAPTURED_WHITE_ROOK; break;
            case w_Knight: piecePixmapKey = PIX_CAPTURED_WHITE_KNIGHT; break;
            case w_Bishop: piecePixmapKey = PIX_CAPTURED_WHITE_BISHOP; break;
            case w_Queen:  piecePixmapKey = PIX_CAPTURED_WHITE_QUEEN; break;
            case w_Pawn:   piecePixmapKey = PIX_CAPTURED_WHITE_PAWN; break;

            default: break;
        }

        QGraphicsPixmapItem* pixmap = mParentScene->addPixmap(Pixmaps::get(piecePixmapKey));

        int x = white ? mMeX : mOpponentX;
        int y = white ? mMeY : mOpponentY;

        pixmap->moveBy(x + i*mWidth, y);
    }

    // draw captured blacks
    for(int i=0; i<black_pieces.size(); i++)
    {
        PixmapKey piecePixmapKey = PIX_NONE;
        switch(black_pieces[i])
        {
            case b_Rook:   piecePixmapKey = PIX_CAPTURED_BLACK_ROOK; break;
            case b_Knight: piecePixmapKey = PIX_CAPTURED_BLACK_KNIGHT; break;
            case b_Bishop: piecePixmapKey = PIX_CAPTURED_BLACK_BISHOP; break;
            case b_Queen:  piecePixmapKey = PIX_CAPTURED_BLACK_QUEEN; break;
            case b_Pawn:   piecePixmapKey = PIX_CAPTURED_BLACK_PAWN; break;

            default: break;
        }

        QGraphicsPixmapItem* pixmap = mParentScene->addPixmap(Pixmaps::get(piecePixmapKey));

        int x = white ? mOpponentX : mMeX;
        int y = white ? mOpponentY : mMeX;

        pixmap->moveBy(x + i*mWidth, y);
    }

}
