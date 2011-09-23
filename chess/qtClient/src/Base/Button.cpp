/*
 * Button.cpp
 *
 *  Created on: Apr 7, 2010
 *      Author: ssy
 */

#include "Button.h"
#include "MainWindow.h"
#include "Defines.h"
#include "XML.h"
#include <QFont>
#include <QDebug>


Button::Button(QGraphicsScene* scene, const QPixmap& pixmap, const QString& text,
               const QString& xmlNodeGroupName, const QString& xmlNodeName):
       QObject(scene),
       QGraphicsPixmapItem(pixmap),
       mScene(scene),
       mXMLNodeName(xmlNodeName),
       mXMLNodeGroupName(xmlNodeGroupName),
       mText(NULL)
{
    mScene->addItem(this);
    setZValue(Z_BUTTONS_LAYER);


    if(text != "")
    {
        QString family = XML::instance().readValue(XML_ITEMS_FILENAME, QList<QString>() << mXMLNodeGroupName << XML_NODE_FONT << XML_NODE_FAMILY);
        int size =       XML::instance().readValue(XML_ITEMS_FILENAME, QList<QString>() << mXMLNodeGroupName  << XML_NODE_FONT << XML_NODE_SIZE).toInt();
        QString color  = XML::instance().readValue(XML_ITEMS_FILENAME, QList<QString>() << mXMLNodeGroupName  << XML_NODE_FONT << XML_NODE_COLOR);

        mText = mScene->addText(text, QFont(family, size));
        mText->setDefaultTextColor( QColor(color) );
        mText->setZValue(Z_TEXT_LAYER);

        mTextOffset = XML::instance().readValue(XML_ITEMS_FILENAME, QList<QString>() << XML_NODE_BUTTONS << XML_NODE_TEXT_OFFSET).toInt();
    }

    if(mXMLNodeName != "" && mXMLNodeGroupName != "")
    {
        int x = XML::instance().readValue(XML_ITEMS_FILENAME, QList<QString>() << mXMLNodeGroupName << mXMLNodeName << XML_NODE_LANDSCAPE << XML_NODE_X).toInt();
        int y = XML::instance().readValue(XML_ITEMS_FILENAME, QList<QString>() << mXMLNodeGroupName << mXMLNodeName << XML_NODE_LANDSCAPE << XML_NODE_Y).toInt();
        this->setPos(x, y);

        if(mText)
        {
            mText->setPos(x + (this->boundingRect().width() - mText->boundingRect().width()) / 2,
                          y + mTextOffset);
        }
    }
}



void Button::mousePressEvent (QGraphicsSceneMouseEvent * event)
{
    if(MainWindow::instance()->getMode() == MW_NORMAL)
    {
        this->setOpacity(OPAQUE_HALF);

        if(mText)
        {
            mText->setOpacity(OPAQUE_HALF);
        }
    }

   // QGraphicsPixmapItem::mousePressEvent(event);
}


void Button::mouseReleaseEvent (QGraphicsSceneMouseEvent * event)
{
    if(MainWindow::instance()->getMode() == MW_NORMAL)
    {
        this->setOpacity(OPAQUE_NORMAL);

        if(mText)
        {
            mText->setOpacity(OPAQUE_NORMAL);
        }

        emit clicked();
    }
}
