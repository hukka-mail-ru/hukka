/*
 * MoveBox.cpp
 *
 *  Created on: Mar 17, 2011
 *      Author: ssy
 */

#include "MoveBox.h"
#include <XML.h>

MoveBox::MoveBox(QGraphicsScene* parentScene, PlayerType playerType)
{
    QString playerNode = (playerType == PT_ME) ? XML_NODE_ME : XML_NODE_OPPONENT;

    // colors
    mActiveColor     = QColor(XML::instance().readValue(XML_ITEMS_FILENAME, QList<QString>() << XML_NODE_MOVE_BOX << playerNode << XML_NODE_COLOR << XML_NODE_ACTIVE));
    mInactiveColor   = QColor(XML::instance().readValue(XML_ITEMS_FILENAME, QList<QString>() << XML_NODE_MOVE_BOX << playerNode << XML_NODE_COLOR << XML_NODE_INACTIVE));

    // border
    int borderX      = XML::instance().readValue(XML_ITEMS_FILENAME, QList<QString>() << XML_NODE_MOVE_BOX << playerNode << XML_NODE_BORDER << XML_NODE_X).toInt();
    int borderY      = XML::instance().readValue(XML_ITEMS_FILENAME, QList<QString>() << XML_NODE_MOVE_BOX << playerNode << XML_NODE_BORDER << XML_NODE_Y).toInt();
    int borderWidth  = XML::instance().readValue(XML_ITEMS_FILENAME, QList<QString>() << XML_NODE_MOVE_BOX << playerNode << XML_NODE_BORDER << XML_NODE_WIDTH).toInt();
    int borderHeight = XML::instance().readValue(XML_ITEMS_FILENAME, QList<QString>() << XML_NODE_MOVE_BOX << playerNode << XML_NODE_BORDER << XML_NODE_HEIGHT).toInt();
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
    int ratingX           = XML::instance().readValue(XML_ITEMS_FILENAME, QList<QString>() << XML_NODE_MOVE_BOX << XML_NODE_RATING << XML_NODE_X).toInt();
    int ratingY           = XML::instance().readValue(XML_ITEMS_FILENAME, QList<QString>() << XML_NODE_MOVE_BOX << XML_NODE_RATING << XML_NODE_Y).toInt();
    QString ratingFamily  = XML::instance().readValue(XML_ITEMS_FILENAME, QList<QString>() << XML_NODE_MOVE_BOX << XML_NODE_RATING << XML_NODE_FONT << XML_NODE_FAMILY);
    int ratingSize        = XML::instance().readValue(XML_ITEMS_FILENAME, QList<QString>() << XML_NODE_MOVE_BOX << XML_NODE_RATING << XML_NODE_FONT << XML_NODE_SIZE).toInt();
    mRatingText = parentScene->addText("", QFont(ratingFamily, ratingSize));
    mRatingText->setDefaultTextColor(mActiveColor);
    mRatingText->setParentItem(mBorder);
    mRatingText->setPos(borderX + ratingX, borderY + ratingY);
}

MoveBox::~MoveBox() {
    // TODO Auto-generated destructor stub
}


void MoveBox::setPlayer(const Player& player)
{
    QString playerColorText = (player.color == PC_WHITE) ? QObject::tr("White") : QObject::tr("Black");
    mPlayerNameText->setPlainText(playerColorText + ": " + player.name);

    QString ratingText = (player.rating == RATING_NOT_AVAILABLE) ? QObject::tr("N/A") : QString::number(player.rating);
    mRatingText->setPlainText(QObject::tr("Rating") + ": " + ratingText);
}

void MoveBox::setActive()
{
    mBorder->setPen(mActiveColor);
    mPlayerNameText->setDefaultTextColor(mActiveColor);
    mRatingText->setDefaultTextColor(mActiveColor);
}

void MoveBox::setInactive()
{
    mBorder->setPen(mInactiveColor);
    mPlayerNameText->setDefaultTextColor(mInactiveColor);
    mRatingText->setDefaultTextColor(mInactiveColor);
}
