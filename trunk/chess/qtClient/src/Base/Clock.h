/*
 * Clock.h
 *
 *  Created on: Jul 5, 2010
 *      Author: ssy
 */

#ifndef CLOCK_H_
#define CLOCK_H_

#include <QLabel>
#include <QTimer>
#include <QGraphicsScene>
#include <QString>
#include <QGraphicsTextItem>
#include <orientation.h>

class Clock: public QObject
{
    Q_OBJECT

public:
    Clock(QGraphicsScene* parentScene, const QString& header,
          const char* updateSignal, const QString& xmlNodeName);

    void start();
   // void getServerTime();

    void moveBy(int x, int y);

    void show();
    void hide();
    void setColor(const QColor& color);


private:

    quint32 mSeconds;
    QTimer *mTimer;

    QGraphicsScene* mParentScene;

    QGraphicsTextItem* mText;

    QString mHeader;
    qint32 mTextFontSize;

    QString mXMLNodeName;

private slots:

    void onTick();
    void onGotTime(quint32 seconds);
    void onGameOver(const QString& message);


};

#endif /* CLOCK_H_ */
