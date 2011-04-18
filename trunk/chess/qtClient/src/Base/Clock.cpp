/*
 * Clock.cpp
 *
 *  Created on: Jul 5, 2010
 *      Author: ssy
 */

#include "Clock.h"
#include <Defines.h>
#include <Client.h>
#include <UI.h>
#include <XML.h>
#include <MainWindow.h>
#include <QDebug>

const int DEFAULT_TIME = 600; // :)

Clock::Clock(QGraphicsScene* parentScene, const QString& header,
             const char* updateSignal, const QString& xmlNodeName):
    mSeconds(DEFAULT_TIME),
    mParentScene(parentScene),
    mText(NULL),
    mHeader(header),
    mXMLNodeName(xmlNodeName),
    mUpdateSignal(updateSignal)
{


    QString family = XML::instance().readValue(XML_ITEMS_FILENAME, QList<QString>() << XML_NODE_MOVE_BOX << mXMLNodeName << XML_NODE_FONT << XML_NODE_FAMILY);
    int size =       XML::instance().readValue(XML_ITEMS_FILENAME, QList<QString>() << XML_NODE_MOVE_BOX << mXMLNodeName << XML_NODE_FONT << XML_NODE_SIZE).toInt();
    int x          = XML::instance().readValue(XML_ITEMS_FILENAME, QList<QString>() << XML_NODE_MOVE_BOX << mXMLNodeName << XML_NODE_X).toInt();
    int y          = XML::instance().readValue(XML_ITEMS_FILENAME, QList<QString>() << XML_NODE_MOVE_BOX << mXMLNodeName << XML_NODE_Y).toInt();

    mText = mParentScene->addText("",QFont(family, size));
    mText->setPos(x, y);
    mText->setZValue(Z_TEXT_LAYER);
    mText->setDefaultTextColor(QColor("white"));

//    qDebug() << "Clock::Clock " << x << y;
    mTimer = new QTimer(this);

}

void Clock::moveBy(int x, int y)
{
    mText->moveBy(x, y);
}

void Clock::start()
{
    connect(Client::instance(), mUpdateSignal, this, SLOT(onGotTime(quint32)));
    connect(mTimer, SIGNAL(timeout()), this, SLOT(onTick()));
    mTimer->start(1000);
}

void Clock::stop()
{
    connect(Client::instance(), mUpdateSignal, this, SLOT(onGotTime(quint32)));
    disconnect(mTimer, SIGNAL(timeout()), this, SLOT(onTick()));
    mTimer->stop();
}

void Clock::show()
{
    mText->setPlainText("");
    mText->show();
}

void Clock::hide()
{
    mText->hide();
}

void Clock::setColor(const QColor& color)
{
    mText->setDefaultTextColor(color);
}



void Clock::onGotTime(quint32 seconds)
{
    mSeconds = seconds;

    if(seconds == INVALID_TIME)
    {
        mSeconds == 0;
        stop();
    }
 //   qDebug() << "onGotTime " << seconds;
}


void Clock::onTick()
{
//qDebug() << "onTick " << mHeader << mSeconds;
    if(mSeconds > 0)
    {
        mSeconds--;
    }
    else
    {
        stop(); // without it, client will be sending timeout instantly
        Client::instance()->timeout(UI::instance()->getGameTable());
    }

    mText->setPlainText(mHeader + Global::seconds2hrs(mSeconds));
}

