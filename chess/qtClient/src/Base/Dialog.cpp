#include <QPainter>
#include <QDebug>
#include <QtGui/QWindowsStyle>

#include "Dialog.h"
#include "Defines.h"
#include "MainWindow.h"

Dialog::Dialog(QWidget *parent, Qt::WindowFlags flags): QDialog(parent, flags)
{
   // resize(MainWindow::instance()->sceneRect().width(), MainWindow::instance()->sceneRect().height()/2);

    setFixedSize(775, 200);

  //  move(MainWindow::instance()->sceneRect().x() + MainWindow::instance()->sceneRect().width()/2 - this->width()/2,
 //           MainWindow::instance()->sceneRect().y() + MainWindow::instance()->sceneRect().height()/2 - this->height()/2     );

    move(9, 0);

    qDebug() << "MainWindow::instance()->sceneRect().width()/2 " << MainWindow::instance()->sceneRect().width()/2;
    qDebug() << "MainWindow::instance()->width()/2 " << MainWindow::instance()->width()/2;

   // No title, no [X] button, no bottom
   setWindowFlags(Qt::Widget);

   // Background
   setStyleSheet("Dialog { background: grey;}");
}

Dialog::~Dialog()
{
    // TODO Auto-generated destructor stub
}


