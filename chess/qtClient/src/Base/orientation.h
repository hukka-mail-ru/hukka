#ifndef ORIENTATION_H
#define ORIENTATION_H

#include <QObject>
#include <QLineEdit>

#include "orientthread.h"

enum OrientationStatus
{
    OrientationVertical,
    OrientationHorizontal
};

class Orientation: public QObject
{
    Q_OBJECT
public:
    Orientation(QObject *parent = 0);
    ~Orientation();

    OrientationStatus getActualOrientation() { return actualOrientation; }

private slots:
    void newSensorsData(QString x, QString y, QString z, QString strSlider);


signals:
    void orientationChanged(OrientationStatus orientation);

private:
    OrientationStatus actualOrientation;
    bool isKeyboardOpen;
    OrientThread* myThread_;

};

#endif // ORIENTATION_H
