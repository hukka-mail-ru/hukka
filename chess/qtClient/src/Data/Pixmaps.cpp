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

    // TODO move constants to XML
    int chatWidth  = XML::instance().readValue(XML_ITEMS_FILENAME, QList<QString>() << XML_NODE_TABLE_CHAT << XML_NODE_WIDTH).toInt();
    int chatHeight = XML::instance().readValue(XML_ITEMS_FILENAME, QList<QString>() << XML_NODE_TABLE_CHAT << XML_NODE_HEIGHT).toInt();

    mPixmaps.insert(PIX_CHAT_BACKGROUND,    mPixmaps[PIX_SPLASH].copy(160, 0, chatWidth, chatHeight ));

    ////////////////////////////////////////////////////////////////////////////////////
    loadPixmap(PIX_CELLS,  ":/images/cells.png");

    int cellWidth = XML::instance().readValue(XML_ITEMS_FILENAME, QList<QString>() << XML_NODE_CELL << XML_NODE_WIDTH).toInt();
    mPixmaps.insert(PIX_CELL_WHITE,    mPixmaps[PIX_CELLS].copy(cellWidth * 0, 0, cellWidth, cellWidth ));
    mPixmaps.insert(PIX_CELL_BLACK,    mPixmaps[PIX_CELLS].copy(cellWidth * 1, 0, cellWidth, cellWidth ));
    mPixmaps.insert(PIX_CELL_WHITE_HIGHLIGHT,mPixmaps[PIX_CELLS].copy(cellWidth * 2, 0, cellWidth, cellWidth ));
    mPixmaps.insert(PIX_CELL_GRAY_HIGHLIGHT,mPixmaps[PIX_CELLS].copy(cellWidth * 3, 0, cellWidth, cellWidth ));


    ////////////////////////////////////////////////////////////////////////////////////
    loadPixmap(PIX_BUTTONS,":/images/buttons.png");

    // load items' properities
    int width  = XML::instance().readValue(XML_ITEMS_FILENAME, QList<QString>() << XML_NODE_BUTTONS << XML_NODE_NEW_GAME << XML_NODE_WIDTH).toInt();
    int height = XML::instance().readValue(XML_ITEMS_FILENAME, QList<QString>() << XML_NODE_BUTTONS << XML_NODE_NEW_GAME << XML_NODE_HEIGHT).toInt();
    mPixmaps.insert(PIX_BUTTON_CREATE_GAME,   mPixmaps[PIX_BUTTONS].copy(width * 0, 0, width, height ));

    width  = XML::instance().readValue(XML_ITEMS_FILENAME, QList<QString>() << XML_NODE_BUTTONS << XML_NODE_FIND_GAME << XML_NODE_WIDTH).toInt();
    height = XML::instance().readValue(XML_ITEMS_FILENAME, QList<QString>() << XML_NODE_BUTTONS << XML_NODE_FIND_GAME << XML_NODE_HEIGHT).toInt();
    mPixmaps.insert(PIX_BUTTON_FIND_GAME,     mPixmaps[PIX_BUTTONS].copy(width * 1, 0, width, height ));

    width  = XML::instance().readValue(XML_ITEMS_FILENAME, QList<QString>() << XML_NODE_BUTTONS << XML_NODE_OPTIONS << XML_NODE_WIDTH).toInt();
    height = XML::instance().readValue(XML_ITEMS_FILENAME, QList<QString>() << XML_NODE_BUTTONS << XML_NODE_OPTIONS << XML_NODE_HEIGHT).toInt();
    mPixmaps.insert(PIX_BUTTON_OPTIONS,       mPixmaps[PIX_BUTTONS].copy(width * 2, 0, width, height ));

    width  = XML::instance().readValue(XML_ITEMS_FILENAME, QList<QString>() << XML_NODE_BUTTONS << XML_NODE_WALLET << XML_NODE_WIDTH).toInt();
    height = XML::instance().readValue(XML_ITEMS_FILENAME, QList<QString>() << XML_NODE_BUTTONS << XML_NODE_WALLET << XML_NODE_HEIGHT).toInt();
    mPixmaps.insert(PIX_BUTTON_WALLET,        mPixmaps[PIX_BUTTONS].copy(width * 3, 0, width, height ));

    ////////////////////////////////////////////////////////////////////////////////////
    loadPixmap(PIX_BUTTON_MENU,":/images/menu.png");
    loadPixmap(PIX_BUTTON_CHAT,":/images/chat.png");
    loadPixmap(PIX_BUTTON_COMMON_CHAT,":/images/commonChat.png");

    loadPixmap(PIX_BUTTON_EXIT,":/images/exit.png");
    ////////////////////////////////////////////////////////////////////////////////////


    loadPixmap(PIX_PROMOTION,":/images/promotion.png");

    int promotionWidth  = XML::instance().readValue(XML_ITEMS_FILENAME, QList<QString>() << XML_NODE_PROMOTION << XML_NODE_WIDTH).toInt();
    int promotionHeight = XML::instance().readValue(XML_ITEMS_FILENAME, QList<QString>() << XML_NODE_PROMOTION << XML_NODE_HEIGHT).toInt();
    mPixmaps.insert(PIX_PROMOTION_WHITE_QUEEN,    mPixmaps[PIX_PROMOTION].copy(promotionWidth * 0, 0, promotionWidth, promotionHeight ));
    mPixmaps.insert(PIX_PROMOTION_WHITE_ROOK,     mPixmaps[PIX_PROMOTION].copy(promotionWidth * 1, 0, promotionWidth, promotionHeight ));
    mPixmaps.insert(PIX_PROMOTION_WHITE_BISHOP,   mPixmaps[PIX_PROMOTION].copy(promotionWidth * 2, 0, promotionWidth, promotionHeight ));
    mPixmaps.insert(PIX_PROMOTION_WHITE_KNIGHT,   mPixmaps[PIX_PROMOTION].copy(promotionWidth * 3, 0, promotionWidth, promotionHeight ));

    mPixmaps.insert(PIX_PROMOTION_BLACK_QUEEN,    mPixmaps[PIX_PROMOTION].copy(promotionWidth * 0, promotionHeight, promotionWidth, promotionHeight ));
    mPixmaps.insert(PIX_PROMOTION_BLACK_ROOK,     mPixmaps[PIX_PROMOTION].copy(promotionWidth * 1, promotionHeight, promotionWidth, promotionHeight ));
    mPixmaps.insert(PIX_PROMOTION_BLACK_BISHOP,   mPixmaps[PIX_PROMOTION].copy(promotionWidth * 2, promotionHeight, promotionWidth, promotionHeight ));
    mPixmaps.insert(PIX_PROMOTION_BLACK_KNIGHT,   mPixmaps[PIX_PROMOTION].copy(promotionWidth * 3, promotionHeight, promotionWidth, promotionHeight ));

    ////////////////////////////////////////////////////////////////////////////////////
    loadPixmap(PIX_PIECES, ":/images/pieces.png");

    int pieceWidth  = XML::instance().readValue(XML_ITEMS_FILENAME, QList<QString>() << XML_NODE_PIECE << XML_NODE_WIDTH).toInt();
    int pieceHeight = XML::instance().readValue(XML_ITEMS_FILENAME, QList<QString>() << XML_NODE_PIECE << XML_NODE_HEIGHT).toInt();

    mPixmaps.insert(PIX_WHITE_ROOK_ON_WHITE,   mPixmaps[PIX_PIECES].copy(pieceWidth * 0, 0, pieceWidth, pieceHeight ));
    mPixmaps.insert(PIX_WHITE_KNIGHT_ON_WHITE, mPixmaps[PIX_PIECES].copy(pieceWidth * 1, 0, pieceWidth, pieceHeight ));
    mPixmaps.insert(PIX_WHITE_BISHOP_ON_WHITE, mPixmaps[PIX_PIECES].copy(pieceWidth * 2, 0, pieceWidth, pieceHeight ));
    mPixmaps.insert(PIX_WHITE_QUEEN_ON_WHITE,  mPixmaps[PIX_PIECES].copy(pieceWidth * 3, 0, pieceWidth, pieceHeight ));
    mPixmaps.insert(PIX_WHITE_KING_ON_WHITE,   mPixmaps[PIX_PIECES].copy(pieceWidth * 4, 0, pieceWidth, pieceHeight ));
    mPixmaps.insert(PIX_WHITE_PAWN_ON_WHITE,   mPixmaps[PIX_PIECES].copy(pieceWidth * 5, 0, pieceWidth, pieceHeight ));

    mPixmaps.insert(PIX_WHITE_ROOK_ON_BLACK,   mPixmaps[PIX_PIECES].copy(pieceWidth * 0, pieceHeight, pieceWidth, pieceHeight ));
    mPixmaps.insert(PIX_WHITE_KNIGHT_ON_BLACK, mPixmaps[PIX_PIECES].copy(pieceWidth * 1, pieceHeight, pieceWidth, pieceHeight ));
    mPixmaps.insert(PIX_WHITE_BISHOP_ON_BLACK, mPixmaps[PIX_PIECES].copy(pieceWidth * 2, pieceHeight, pieceWidth, pieceHeight ));
    mPixmaps.insert(PIX_WHITE_QUEEN_ON_BLACK,  mPixmaps[PIX_PIECES].copy(pieceWidth * 3, pieceHeight, pieceWidth, pieceHeight ));
    mPixmaps.insert(PIX_WHITE_KING_ON_BLACK,   mPixmaps[PIX_PIECES].copy(pieceWidth * 4, pieceHeight, pieceWidth, pieceHeight ));
    mPixmaps.insert(PIX_WHITE_PAWN_ON_BLACK,   mPixmaps[PIX_PIECES].copy(pieceWidth * 5, pieceHeight, pieceWidth, pieceHeight ));


    mPixmaps.insert(PIX_BLACK_ROOK_ON_WHITE,   mPixmaps[PIX_PIECES].copy(pieceWidth * 0, pieceHeight*2, pieceWidth, pieceHeight ));
    mPixmaps.insert(PIX_BLACK_KNIGHT_ON_WHITE, mPixmaps[PIX_PIECES].copy(pieceWidth * 1, pieceHeight*2, pieceWidth, pieceHeight ));
    mPixmaps.insert(PIX_BLACK_BISHOP_ON_WHITE, mPixmaps[PIX_PIECES].copy(pieceWidth * 2, pieceHeight*2, pieceWidth, pieceHeight ));
    mPixmaps.insert(PIX_BLACK_QUEEN_ON_WHITE,  mPixmaps[PIX_PIECES].copy(pieceWidth * 3, pieceHeight*2, pieceWidth, pieceHeight ));
    mPixmaps.insert(PIX_BLACK_KING_ON_WHITE,   mPixmaps[PIX_PIECES].copy(pieceWidth * 4, pieceHeight*2, pieceWidth, pieceHeight ));
    mPixmaps.insert(PIX_BLACK_PAWN_ON_WHITE,   mPixmaps[PIX_PIECES].copy(pieceWidth * 5, pieceHeight*2, pieceWidth, pieceHeight ));

    mPixmaps.insert(PIX_BLACK_ROOK_ON_BLACK,   mPixmaps[PIX_PIECES].copy(pieceWidth * 0, pieceHeight*3, pieceWidth, pieceHeight ));
    mPixmaps.insert(PIX_BLACK_KNIGHT_ON_BLACK, mPixmaps[PIX_PIECES].copy(pieceWidth * 1, pieceHeight*3, pieceWidth, pieceHeight ));
    mPixmaps.insert(PIX_BLACK_BISHOP_ON_BLACK, mPixmaps[PIX_PIECES].copy(pieceWidth * 2, pieceHeight*3, pieceWidth, pieceHeight ));
    mPixmaps.insert(PIX_BLACK_QUEEN_ON_BLACK,  mPixmaps[PIX_PIECES].copy(pieceWidth * 3, pieceHeight*3, pieceWidth, pieceHeight ));
    mPixmaps.insert(PIX_BLACK_KING_ON_BLACK,   mPixmaps[PIX_PIECES].copy(pieceWidth * 4, pieceHeight*3, pieceWidth, pieceHeight ));
    mPixmaps.insert(PIX_BLACK_PAWN_ON_BLACK,   mPixmaps[PIX_PIECES].copy(pieceWidth * 5, pieceHeight*3, pieceWidth, pieceHeight ));

    ////////////////////////////////////////////////////////////////////////////////////
    loadPixmap(PIX_CAPTURED, ":/images/captured.png");

    int capturedWidth  = XML::instance().readValue(XML_ITEMS_FILENAME, QList<QString>() << XML_NODE_CAPTURE_BOX << XML_NODE_PIECE << XML_NODE_WIDTH).toInt();
    int capturedHeight = XML::instance().readValue(XML_ITEMS_FILENAME, QList<QString>() << XML_NODE_CAPTURE_BOX << XML_NODE_PIECE << XML_NODE_HEIGHT).toInt();

    mPixmaps.insert(PIX_CAPTURED_WHITE_QUEEN,  mPixmaps[PIX_CAPTURED].copy(capturedWidth * 0, 0, capturedWidth, capturedHeight ));
    mPixmaps.insert(PIX_CAPTURED_WHITE_ROOK,   mPixmaps[PIX_CAPTURED].copy(capturedWidth * 1, 0, capturedWidth, capturedHeight ));
    mPixmaps.insert(PIX_CAPTURED_WHITE_BISHOP, mPixmaps[PIX_CAPTURED].copy(capturedWidth * 2, 0, capturedWidth, capturedHeight ));
    mPixmaps.insert(PIX_CAPTURED_WHITE_KNIGHT, mPixmaps[PIX_CAPTURED].copy(capturedWidth * 3, 0, capturedWidth, capturedHeight ));
    mPixmaps.insert(PIX_CAPTURED_WHITE_PAWN,   mPixmaps[PIX_CAPTURED].copy(capturedWidth * 4, 0, capturedWidth, capturedHeight ));

    mPixmaps.insert(PIX_CAPTURED_BLACK_QUEEN,  mPixmaps[PIX_CAPTURED].copy(capturedWidth * 0, capturedHeight, capturedWidth, capturedHeight ));
    mPixmaps.insert(PIX_CAPTURED_BLACK_ROOK,   mPixmaps[PIX_CAPTURED].copy(capturedWidth * 1, capturedHeight, capturedWidth, capturedHeight ));
    mPixmaps.insert(PIX_CAPTURED_BLACK_BISHOP, mPixmaps[PIX_CAPTURED].copy(capturedWidth * 2, capturedHeight, capturedWidth, capturedHeight ));
    mPixmaps.insert(PIX_CAPTURED_BLACK_KNIGHT, mPixmaps[PIX_CAPTURED].copy(capturedWidth * 3, capturedHeight, capturedWidth, capturedHeight ));
    mPixmaps.insert(PIX_CAPTURED_BLACK_PAWN,   mPixmaps[PIX_CAPTURED].copy(capturedWidth * 4, capturedHeight, capturedWidth, capturedHeight ));

    ////////////////////////////////////////////////////////////////////////////////////
    loadPixmap(PIX_CAPTURED_BOX_WHITE, ":/images/capturedBoxWhite.png");
    loadPixmap(PIX_CAPTURED_BOX_BLACK, ":/images/capturedBoxBlack.png");


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
