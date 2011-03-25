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
    mXMLNodeName(xmlNodeName)
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

    connect(Client::instance(), updateSignal, this, SLOT(onGotTime(quint32)));
    connect(mTimer, SIGNAL(timeout()), this, SLOT(onTick()));

}

void Clock::moveBy(int x, int y)
{
    mText->moveBy(x, y);
}

void Clock::start()
{
//    qDebug() << "Clock::start";
    connect(Client::instance(), SIGNAL(gameOver(const QString&)), this, SLOT(onGameOver(const QString&)));
    mTimer->start(1000);
}

void Clock::show()
{
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


void Clock::getServerTime()
{
    Client::instance()->getTime(UI::instance()->getGameTable());
}


void Clock::onGotTime(quint32 seconds)
{
    mSeconds = seconds;

    if(seconds == INVALID_TIME)
    {
        mSeconds == 0;
        mTimer->stop();
    }
 //   qDebug() << "onGotTime " << seconds;
}


void Clock::onTick()
{

    if(mSeconds > 0)
    {
        mSeconds--;
    }
    else
    {
        Client::instance()->timeout(UI::instance()->getGameTable());
    }

    mText->setPlainText(mHeader + Global::seconds2hrs(mSeconds));
}

void Clock::onGameOver(const QString& message)
{
    disconnect(Client::instance(), SIGNAL(gameOver(const QString&)), this, SLOT(onGameOver(const QString&)));
    mTimer->stop();
}
