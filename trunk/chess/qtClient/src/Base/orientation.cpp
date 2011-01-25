#include "orientation.h"
#include <QDebug>
#include <QFile>
#include <QThread>

#define Z_ROTATION_ACCURACY 850

Orientation::Orientation(QObject *parent)
{
    myThread_ = new OrientThread(this);
    connect(myThread_, SIGNAL(deviceOrientation(QString,QString,QString, QString)), this, SLOT(newSensorsData(QString,QString,QString, QString)));

    myThread_->start(QThread::NormalPriority);
}

Orientation::~Orientation()
{
    myThread_->quit();

    for(;;)
    {
        if(myThread_->isFinished())
            break;
    }

    delete myThread_;
}

void Orientation::newSensorsData(QString strX, QString strY, QString strZ, QString strSlider)
{
    qreal x = strX.toDouble();
    qreal y = strY.toDouble();
    qreal z = strZ.toDouble();

    if (abs(z) > Z_ROTATION_ACCURACY)
        return;

    if (!isKeyboardOpen)
    {
        if (abs(x) < abs(y))
        {
            if (y < 0 && actualOrientation != OrientationHorizontal)
            {
                actualOrientation = OrientationHorizontal;
                emit orientationChanged(OrientationHorizontal);
            }
        }

        if (abs(x) > abs(y))
        {

            if (x < 0 && actualOrientation != OrientationVertical)
            {
                actualOrientation = OrientationVertical;
                emit orientationChanged(OrientationVertical);
            }
        }
    }

    if (strSlider.contains("open") && (!isKeyboardOpen) && actualOrientation != OrientationHorizontal)
    {
        isKeyboardOpen = true;
        actualOrientation = OrientationHorizontal;
        emit orientationChanged(OrientationHorizontal);
    }

    if (strSlider.contains("closed") && isKeyboardOpen)
    {
        isKeyboardOpen = false;
    }

}
