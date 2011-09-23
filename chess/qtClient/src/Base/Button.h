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
    Button(QGraphicsScene* scene, const QPixmap& pixmap, const QString& text,
           const QString& xmlNodeGroupName, const QString& xmlNodeName);

signals:
    void clicked();


protected:

   virtual void mousePressEvent (QGraphicsSceneMouseEvent * event);
   virtual void mouseReleaseEvent (QGraphicsSceneMouseEvent * event);

private:

    int mTextOffset;

    QGraphicsScene* mScene;

    QString mXMLNodeName;
    QString mXMLNodeGroupName;
    QGraphicsTextItem* mText;

};



#endif /* BUTTON_H_ */
