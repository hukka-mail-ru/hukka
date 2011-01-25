#include "orientthread.h"
#include <QTimer>
#include <QFile>
#include <QTextStream>
#include <QDebug>
#include <QStringList>


OrientThread::OrientThread(QObject *parent) :
    QThread(parent)
{
    updateCoords();
}

void OrientThread::run()
{
    qDebug() << "OrientThread::run";

    QTimer timer;
    connect(&timer, SIGNAL(timeout()), this, SLOT(updateCoords()));
    timer.start(50);
    exec();
}

void OrientThread::updateCoords()
{
     QFile file(ACCELERATION_PATH);
     if (!file.open(QIODevice::ReadOnly | QIODevice::Text))
         return;
     QFile file2(KEYBOARD_SLIDER_STATUS_PATH);
     if (!file2.open(QIODevice::ReadOnly | QIODevice::Text))
         return;
     QTextStream in(&file);
     QTextStream sl(&file2);
     QString accdata = in.readAll();
     QString slider = sl.readAll();
     processCoords(accdata, slider);
}

void OrientThread::processCoords(QString &accdata, QString &slider)
{
    QStringList data_splited = accdata.split(" ");

    x = data_splited[0];
    y = data_splited[1];
    z = data_splited[2];
    s = slider;

    emit deviceOrientation(x, y, z, s);
}
