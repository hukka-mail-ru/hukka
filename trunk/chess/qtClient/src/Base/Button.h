#ifndef BUTTON123_H_
#define BUTTON123_H_

#include <QObject>
#include <QGraphicsWidget>
#include <QGraphicsTextItem>
#include <orientation.h>

class Button: public QObject, public QGraphicsPixmapItem
{
Q_OBJECT
public:
    Button(QGraphicsScene* scene, const QPixmap& pixmap, const QString& text, const QString& xmlNodeName);

    void updatePos(OrientationStatus orientation);

    static int width() { return mWidth; }
    static int height() { return mHeight; }

    static void setWidth(int width) { mWidth = width; }
    static void setHeight(int height) { mHeight = height; }
    static void setTextOffset(int offset) { mTextOffset = offset; }

signals:
    void clicked();


protected:

   virtual void mousePressEvent (QGraphicsSceneMouseEvent * event);
   virtual void mouseReleaseEvent (QGraphicsSceneMouseEvent * event);

private:

    static int mWidth;
    static int mHeight;
    static int mTextOffset;

    QGraphicsScene* mScene;

    QString mXMLNodeName;
    QGraphicsTextItem* mText;

};



#endif /* BUTTON_H_ */
