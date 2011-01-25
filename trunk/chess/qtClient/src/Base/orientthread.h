#ifndef ORIENTTHREAD_H
#define ORIENTTHREAD_H

#include <QThread>

//#define ACCELERATION_PATH "/sys/class/i2c-adapter/i2c-3/3-001d/coord"
//#define KEYBOARD_SLIDER_STATUS_PATH "/sys/devices/platform/gpio-switch/slide/state"

#define ACCELERATION_PATH "../config/coord"
#define KEYBOARD_SLIDER_STATUS_PATH "../config/slider"

class OrientThread : public QThread
{
    Q_OBJECT
public:
    explicit OrientThread(QObject *parent = 0);
    void run();

    QString x;
    QString y;
    QString z;
    QString s;

public slots:
    void updateCoords();
    void processCoords(QString &accdata, QString &slider);

signals:
    void deviceOrientation(QString x, QString y, QString z, QString s);

};

#endif // ORIENTTHREAD_H
