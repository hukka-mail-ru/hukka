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

class Clock: public QObject
{
    Q_OBJECT

public:
    Clock(QGraphicsScene* parentScene, const QString& header,
          const char* updateSignal, const QString& xmlNodeName);

    void start();
    void stop();
   // void getServerTime();

    void moveBy(int x, int y);

    void show();
    void hide();
    void setColor(const QColor& color);


private:

    unsigned mSeconds;
    QTimer *mTimer;

    QGraphicsScene* mParentScene;

    QGraphicsTextItem* mText;

    QString mHeader;
    int mTextFontSize;

    QString mXMLNodeName;

    const char* mUpdateSignal;

private slots:

    void onTick();
    void onGotTime(unsigned seconds);


};

#endif /* CLOCK_H_ */
