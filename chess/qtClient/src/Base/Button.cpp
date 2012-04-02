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


    if(mXMLNodeName != "" && mXMLNodeGroupName != "")
    {
        int x = XML::instance().readValue(XML_ITEMS_FILENAME, QList<QString>() << mXMLNodeGroupName << mXMLNodeName << XML_NODE_X).toInt();
        int y = XML::instance().readValue(XML_ITEMS_FILENAME, QList<QString>() << mXMLNodeGroupName << mXMLNodeName << XML_NODE_Y).toInt();
        this->setPos(x, y);

        if(text != "")
        {
            QString family = XML::instance().readValue(XML_ITEMS_FILENAME, QList<QString>() << mXMLNodeGroupName << mXMLNodeName << XML_NODE_FONT << XML_NODE_FAMILY);
            int size =       XML::instance().readValue(XML_ITEMS_FILENAME, QList<QString>() << mXMLNodeGroupName << mXMLNodeName << XML_NODE_FONT << XML_NODE_SIZE).toInt();
            QString color  = XML::instance().readValue(XML_ITEMS_FILENAME, QList<QString>() << mXMLNodeGroupName << mXMLNodeName << XML_NODE_FONT << XML_NODE_COLOR);
            int textX      = XML::instance().readValue(XML_ITEMS_FILENAME, QList<QString>() << mXMLNodeGroupName << mXMLNodeName << XML_NODE_FONT << XML_NODE_X).toInt();
            int textY      = XML::instance().readValue(XML_ITEMS_FILENAME, QList<QString>() << mXMLNodeGroupName << mXMLNodeName << XML_NODE_FONT << XML_NODE_Y).toInt();

            mText = mScene->addText(text, QFont(family, size));
            mText->setDefaultTextColor( QColor(color) );
            mText->setZValue(Z_TEXT_LAYER);


        	if(textX == -1) // -1 is default
        	{
        		textX = x + (this->boundingRect().width() - mText->boundingRect().width()) / 2;
        	}

        	textY = y + textY;

            mText->setPos(textX, textY);
        }
    }
}



void Button::mousePressEvent (QGraphicsSceneMouseEvent * event)
{
    Q_UNUSED(event);
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
    Q_UNUSED(event);
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
