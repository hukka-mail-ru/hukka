/*
 * MoveBox.cpp
 *
 *  Created on: Mar 17, 2011
 *      Author: ssy
 */

#include "MoveBox.h"
#include <XML.h>
#include <UI.h>
#include <QObject>

MoveBox::MoveBox(QGraphicsScene* parentScene):
    mMoveClock(parentScene, QObject::tr(""), SIGNAL(gotMoveTime(unsigned)), XML_NODE_MOVE_CLOCK),
    mGameClock(parentScene, QObject::tr("Game: "), SIGNAL(gotGameTime(unsigned)), XML_NODE_GAME_CLOCK)
{
    // colors
    mActiveColor     = QColor(XML::instance().readValue(XML_ITEMS_FILENAME, QList<QString>() << XML_NODE_MOVE_BOX << XML_NODE_COLOR << XML_NODE_ACTIVE));
    mInactiveColor   = QColor(XML::instance().readValue(XML_ITEMS_FILENAME, QList<QString>() << XML_NODE_MOVE_BOX  << XML_NODE_COLOR << XML_NODE_INACTIVE));

    // border
    int borderX      = XML::instance().readValue(XML_ITEMS_FILENAME, QList<QString>() << XML_NODE_MOVE_BOX  << XML_NODE_BORDER << XML_NODE_X).toInt();
    int borderY      = XML::instance().readValue(XML_ITEMS_FILENAME, QList<QString>() << XML_NODE_MOVE_BOX  << XML_NODE_BORDER << XML_NODE_Y).toInt();
    int borderWidth  = XML::instance().readValue(XML_ITEMS_FILENAME, QList<QString>() << XML_NODE_MOVE_BOX  << XML_NODE_BORDER << XML_NODE_WIDTH).toInt();
    int borderHeight = XML::instance().readValue(XML_ITEMS_FILENAME, QList<QString>() << XML_NODE_MOVE_BOX  << XML_NODE_BORDER << XML_NODE_HEIGHT).toInt();
    mBorder = parentScene->addRect (borderX, borderY, borderWidth, borderHeight, QPen(mActiveColor));

    // Player Name

    int playerNameX           = XML::instance().readValue(XML_ITEMS_FILENAME, QList<QString>() << XML_NODE_MOVE_BOX << XML_NODE_PLAYER << XML_NODE_X).toInt();
    int playerNameY           = XML::instance().readValue(XML_ITEMS_FILENAME, QList<QString>() << XML_NODE_MOVE_BOX << XML_NODE_PLAYER << XML_NODE_Y).toInt();
    QString playerNameFamily  = XML::instance().readValue(XML_ITEMS_FILENAME, QList<QString>() << XML_NODE_MOVE_BOX << XML_NODE_PLAYER << XML_NODE_FONT << XML_NODE_FAMILY);
    int playerNameSize        = XML::instance().readValue(XML_ITEMS_FILENAME, QList<QString>() << XML_NODE_MOVE_BOX << XML_NODE_PLAYER << XML_NODE_FONT << XML_NODE_SIZE).toInt();
    mPlayerNameText = parentScene->addText("", QFont(playerNameFamily, playerNameSize));
    mPlayerNameText->setDefaultTextColor(mActiveColor);
    mPlayerNameText->setParentItem(mBorder);
    mPlayerNameText->setPos(borderX + playerNameX, borderY + playerNameY);

    // Player Rating
/*    int ratingX           = XML::instance().readValue(XML_ITEMS_FILENAME, QList<QString>() << XML_NODE_MOVE_BOX << XML_NODE_RATING << XML_NODE_X).toInt();
    int ratingY           = XML::instance().readValue(XML_ITEMS_FILENAME, QList<QString>() << XML_NODE_MOVE_BOX << XML_NODE_RATING << XML_NODE_Y).toInt();
    QString ratingFamily  = XML::instance().readValue(XML_ITEMS_FILENAME, QList<QString>() << XML_NODE_MOVE_BOX << XML_NODE_RATING << XML_NODE_FONT << XML_NODE_FAMILY);
    int ratingSize        = XML::instance().readValue(XML_ITEMS_FILENAME, QList<QString>() << XML_NODE_MOVE_BOX << XML_NODE_RATING << XML_NODE_FONT << XML_NODE_SIZE).toInt();
    mRatingText = parentScene->addText("", QFont(ratingFamily, ratingSize));
    mRatingText->setDefaultTextColor(mActiveColor);
    mRatingText->setParentItem(mBorder);
    mRatingText->setPos(borderX + ratingX, borderY + ratingY);
*/
    mMoveClock.moveBy(borderX, borderY);
    mGameClock.moveBy(borderX, borderY);
}

MoveBox::~MoveBox() {
    // TODO Auto-generated destructor stub
}



void MoveBox::setPlayer(PlayerType type)
{
    QString text = "";
    if(type == PT_ME)
    {
        text = QObject::tr("Your move!");
    }
    else if(type == PT_OPPONENT)
    {
        Player opponent = UI::instance()->getPlayer(type);
        QString oppColorText = (opponent.color == PC_WHITE) ? QObject::tr("white") : QObject::tr("black");
        text = QObject::tr("Move: ") + oppColorText + " (" + opponent.name + ")";
    }
    mPlayerNameText->setPlainText(text);

//    QString ratingText = (player.rating == RATING_NOT_AVAILABLE) ? QObject::tr("N/A") : QString::number(player.rating);
//    mRatingText->setPlainText(QObject::tr("Rating") + ": " + ratingText);
}

void MoveBox::setActive()
{
  //  mMoveClock.getServerTime();
    mMoveClock.start();
    mGameClock.start();

    mMoveClock.show();
    mGameClock.show();

    mMoveClock.setColor(mActiveColor);
    mGameClock.setColor(mActiveColor);

    mBorder->setPen(mActiveColor);
    mPlayerNameText->setDefaultTextColor(mActiveColor);
//    mRatingText->setDefaultTextColor(mActiveColor);
}

void MoveBox::setInactive()
{
    mMoveClock.stop();
    mGameClock.stop();

    mMoveClock.hide();
    mGameClock.hide();

    mBorder->setPen(mInactiveColor);
    mPlayerNameText->setDefaultTextColor(mInactiveColor);
//    mRatingText->setDefaultTextColor(mInactiveColor);
}

void MoveBox::setGameOver()
{
    mPlayerNameText->setPlainText(QObject::tr("Game over."));
}
