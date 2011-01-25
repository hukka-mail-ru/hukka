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

int Button::mWidth = 0;
int Button::mHeight = 0;
int Button::mTextOffset = 0;

Button::Button(QGraphicsScene* scene, const QPixmap& pixmap, const QString& text, const QString& xmlNodeName):
       QGraphicsPixmapItem(pixmap),
       mScene(scene),
       mXMLNodeName(xmlNodeName),
       mText(NULL)
{
    mScene->addItem(this);
    setZValue(Z_BUTTONS_LAYER);

    if(text != "")
    {
        QList<QString> path;
        QString family = XML::instance().readValue(XML_ITEMS_FILENAME, path << XML_NODE_BUTTONS << XML_NODE_FONT << XML_NODE_FAMILY); path.clear();
        int size =       XML::instance().readValue(XML_ITEMS_FILENAME, path << XML_NODE_BUTTONS  << XML_NODE_FONT << XML_NODE_SIZE).toInt(); path.clear();
        QString color  = XML::instance().readValue(XML_ITEMS_FILENAME, path << XML_NODE_BUTTONS  << XML_NODE_FONT << XML_NODE_COLOR); path.clear();

        mText = mScene->addText(text, QFont(family, size));
        mText->setDefaultTextColor( QColor(color) );
        mText->setZValue(Z_TEXT_LAYER);
    }

}

void Button::updatePos(OrientationStatus orientation)
{
    QString orientNode = (orientation == OrientationHorizontal) ? XML_NODE_LANDSCAPE : XML_NODE_PORTRAIT;

    QList<QString> path;
    int x = XML::instance().readValue(XML_ITEMS_FILENAME, path << XML_NODE_BUTTONS << mXMLNodeName << orientNode << XML_NODE_X).toInt(); path.clear();
    int y = XML::instance().readValue(XML_ITEMS_FILENAME, path << XML_NODE_BUTTONS << mXMLNodeName << orientNode << XML_NODE_Y).toInt(); path.clear();

    /*
    qDebug() << "x y"  << x << y ;
    qDebug() << XML_ITEMS_FILENAME  << mXMLNodeName << orientNodeX ;
    qDebug() << XML_ITEMS_FILENAME  << mXMLNodeName << orientNodeY ;
    qDebug() << mXMLNodeName << this->pos().x() << this->pos().y() << isVisible() ;
*/

    // let item has x=10, y=10.
    // if we moveBy(20,20), it will have y=30, y=30
    this->moveBy(x - pos().x(), y - pos().y());

//    qDebug() << "after";

    if(mText)
    {
        mText->setPos(x + (this->boundingRect().width() - mText->boundingRect().width()) / 2,
                      y + mTextOffset);
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

        qDebug() << "emit clicked";
        emit clicked();
    }

   // QGraphicsPixmapItem::mouseReleaseEvent(event);
}
