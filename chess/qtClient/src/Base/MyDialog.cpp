#include <QPainter>
#include <QDebug>
#include <QtGui/QWindowsStyle>

#include "MyDialog.h"
#include "Defines.h"
#include "MainWindow.h"
#include "XML.h"

MyDialog::MyDialog(QWidget *parent, Qt::WindowFlags flags): QDialog(parent, flags)
{
    const int margin = 1; // tiny window margin

   // setFixedSize(MainWindow::instance()->width() - 2*margin, MainWindow::instance()->height()/2 - margin);
   // move(margin, MainWindow::instance()->height()/2);
    setFixedSize(MainWindow::instance()->width() - 2*margin, MainWindow::instance()->height() - margin);
    move(margin, margin);

    // No title, no [X] button, no bottom
    setWindowFlags(Qt::Widget);

    // Background
    QString style =  XML::instance().readValue(XML_ITEMS_FILENAME, QList<QString>() << XML_NODE_DIALOGS << XML_NODE_STYLE);
    setStyleSheet(style);
}

MyDialog::~MyDialog()
{
    // TODO Auto-generated destructor stub
}


