/*
 * Game.cpp
 *
 *  Created on: May 19, 2010
 *      Author: ssy
 */
#include "Defines.h"
#include "Exception.h"
#include "Pixmaps.h"
#include <Button.h>
#include <Cell.h>
#include <XML.h>

PixmapHash Pixmaps::mPixmaps;

void Pixmaps::loadPixmaps()
{
    loadPixmap(PIX_SPLASH, ":/images/splash.png");


    ////////////////////////////////////////////////////////////////////////////////////
    loadPixmap(PIX_CELLS,  ":/images/cells.png");

    int cellWidth = Cell::width();
    mPixmaps.insert(PIX_CELL_WHITE,    mPixmaps[PIX_CELLS].copy(cellWidth * 0, 0, cellWidth, cellWidth ));
    mPixmaps.insert(PIX_CELL_BLACK,    mPixmaps[PIX_CELLS].copy(cellWidth * 1, 0, cellWidth, cellWidth ));
    mPixmaps.insert(PIX_CELL_HIGHLIGHT,mPixmaps[PIX_CELLS].copy(cellWidth * 2, 0, cellWidth, cellWidth ));


    ////////////////////////////////////////////////////////////////////////////////////
    loadPixmap(PIX_BUTTONS,":/images/buttons.png");

    int width = Button::width();
    int height = Button::height();
    mPixmaps.insert(PIX_BUTTON_CREATE_GAME,   mPixmaps[PIX_BUTTONS].copy(width * 0, 0, width, height ));
    mPixmaps.insert(PIX_BUTTON_FIND_GAME,     mPixmaps[PIX_BUTTONS].copy(width * 1, 0, width, height ));
    mPixmaps.insert(PIX_BUTTON_CHAT,          mPixmaps[PIX_BUTTONS].copy(width * 2, 0, width, height ));
    mPixmaps.insert(PIX_BUTTON_OPTIONS,       mPixmaps[PIX_BUTTONS].copy(width * 3, 0, width, height ));
    mPixmaps.insert(PIX_BUTTON_MENU,          mPixmaps[PIX_BUTTONS].copy(width * 4, 0, width, height ));

    loadPixmap(PIX_BUTTON_EXIT,":/images/exit.png");

    ////////////////////////////////////////////////////////////////////////////////////
    loadPixmap(PIX_PIECES, ":/images/pieces.png");

    int pieceWidth  = XML::instance().readValue(XML_ITEMS_FILENAME, QList<QString>() << XML_NODE_PIECE << XML_NODE_WIDTH).toInt();
    int pieceHeight = XML::instance().readValue(XML_ITEMS_FILENAME, QList<QString>() << XML_NODE_PIECE << XML_NODE_HEIGHT).toInt();

    mPixmaps.insert(PIX_WHITE_KING,   mPixmaps[PIX_PIECES].copy(pieceWidth * 0, 0, pieceWidth, pieceHeight ));
    mPixmaps.insert(PIX_WHITE_QUEEN,  mPixmaps[PIX_PIECES].copy(pieceWidth * 1, 0, pieceWidth, pieceHeight ));
    mPixmaps.insert(PIX_WHITE_BISHOP, mPixmaps[PIX_PIECES].copy(pieceWidth * 2, 0, pieceWidth, pieceHeight ));
    mPixmaps.insert(PIX_WHITE_KNIGHT, mPixmaps[PIX_PIECES].copy(pieceWidth * 3, 0, pieceWidth, pieceHeight ));
    mPixmaps.insert(PIX_WHITE_ROOK,   mPixmaps[PIX_PIECES].copy(pieceWidth * 4, 0, pieceWidth, pieceHeight ));
    mPixmaps.insert(PIX_WHITE_PAWN,   mPixmaps[PIX_PIECES].copy(pieceWidth * 5, 0, pieceWidth, pieceHeight ));

    mPixmaps.insert(PIX_BLACK_KING,   mPixmaps[PIX_PIECES].copy(pieceWidth * 0, pieceHeight, pieceWidth, pieceHeight ));
    mPixmaps.insert(PIX_BLACK_QUEEN,  mPixmaps[PIX_PIECES].copy(pieceWidth * 1, pieceHeight, pieceWidth, pieceHeight ));
    mPixmaps.insert(PIX_BLACK_BISHOP, mPixmaps[PIX_PIECES].copy(pieceWidth * 2, pieceHeight, pieceWidth, pieceHeight ));
    mPixmaps.insert(PIX_BLACK_KNIGHT, mPixmaps[PIX_PIECES].copy(pieceWidth * 3, pieceHeight, pieceWidth, pieceHeight ));
    mPixmaps.insert(PIX_BLACK_ROOK,   mPixmaps[PIX_PIECES].copy(pieceWidth * 4, pieceHeight, pieceWidth, pieceHeight ));
    mPixmaps.insert(PIX_BLACK_PAWN,   mPixmaps[PIX_PIECES].copy(pieceWidth * 5, pieceHeight, pieceWidth, pieceHeight ));

}

void Pixmaps::loadPixmap(PixmapKey key, const QString& filename)
{
    QPixmap pixmap;
    if(!pixmap.load(filename))
    {
        THROW_EXCEPTION("Can't load image: " + filename);
    }

    mPixmaps.insert(key, pixmap);
}

QPixmap&  Pixmaps::get(PixmapKey key)
{
    return mPixmaps[key];
}
