#include <QPainter>
#include <QDebug>
#include <QtGui/QWindowsStyle>

#include "MyDialog.h"
#include "Defines.h"
#include "MainWindow.h"

MyDialog::MyDialog(QWidget *parent, Qt::WindowFlags flags): QDialog(parent, flags)
{
    setFixedSize(MainWindow::instance()->width(), MainWindow::instance()->height()/2);
    move(0, MainWindow::instance()->height()/2);

    // No title, no [X] button, no bottom
    setWindowFlags(Qt::Widget);

    // Background
    setStyleSheet("MyDialog    { background: grey;} "
                  "QLineEdit   { font-size: 18px; } "
                  "QLabel      { font-size: 18px; color: white; }"
                  "QCheckBox   { font-size: 18px; color: white; }"
                  "QComboBox   { font-size: 18px; }"
                  "QPushButton { font-size: 18px; }"
                  "QTableWidget { font-size: 18px; }");
}

MyDialog::~MyDialog()
{
    // TODO Auto-generated destructor stub
}


